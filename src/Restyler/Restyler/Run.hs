{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
--
-- Module      : Restyler.Restyler.Run
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Restyler.Run
  ( runRestylers

    -- * Errors
  , RestylerPullFailure (..)
  , RestylerExitFailure (..)
  , RestylerOutOfMemory (..)
  , RestylerCommandNotFound (..)

    -- * Exported for testing only
  , runRestyler
  , withFilteredPaths
  ) where

import Restyler.Prelude

import Control.Retry
  ( RetryPolicyM
  , RetryStatus (..)
  , exponentialBackoff
  , limitRetries
  , retrying
  )
import Data.List (nub)
import Data.Text qualified as T
import Restyler.AnnotatedException
import Restyler.CodeVolume
import Restyler.Config
import Restyler.Config.Glob (matchPath)
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Delimited
import Restyler.Monad.Directory
import Restyler.Monad.Docker
import Restyler.Monad.DownloadFile
import Restyler.Monad.Git
import Restyler.Monad.ReadFile
import Restyler.Monad.WriteFile
import Restyler.Path
import Restyler.Restyler
import Restyler.RestylerResult

data RestylerPullFailure = RestylerPullFailure Restyler Int
  deriving stock (Eq, Show)

instance Exception RestylerPullFailure where
  displayException (RestylerPullFailure Restyler {..} ec) =
    mconcat
      [ "Unable to pull: " <> rImage <> " (exit " <> show @String ec <> ")"
      , "\nThe source of the error may be visible in debug logging"
      ]

data RestylerExitFailure = RestylerExitFailure Restyler Int
  deriving stock (Eq, Show)

instance Exception RestylerExitFailure where
  displayException (RestylerExitFailure Restyler {..} ec) =
    mconcat
      [ "Restyler " <> rName <> " exited non-zero (" <> show @String ec <> ")"
      , "\nError information may be present in debug messages printed above"
      , "\n"
      , "\nHelp:"
      , concatMap ("\n  " <>) rDocumentation
      ]

newtype RestylerOutOfMemory = RestylerOutOfMemory Restyler
  deriving stock (Eq, Show)

instance Exception RestylerOutOfMemory where
  displayException (RestylerOutOfMemory Restyler {..}) =
    mconcat
      [ "Restyler " <> rName <> " used too much memory (exit code 137)"
      ]

newtype RestylerCommandNotFound = RestylerCommandNotFound Restyler
  deriving stock (Eq, Show)

instance Exception RestylerCommandNotFound where
  displayException (RestylerCommandNotFound Restyler {..}) =
    mconcat
      [ "Restyler " <> rName <> " has an invalid command (exit code 127)"
      , "\n"
      , "You may need to adjust restylers[" <> rName <> "].command"
      ]

-- | Runs the configured @'Restyler'@s for the files and reports results
runRestylers
  :: ( HasCallStack
     , HasCommitTemplate env
     , HasCopyFiles env
     , HasDryRun env
     , HasExclude env
     , HasImageCleanup env
     , HasManifest env
     , HasNoCommit env
     , HasNoPull env
     , HasRemoteFiles env
     , HasRestrictions env
     , HasRestylerOverrides env
     , HasRestylersVersion env
     , MonadDirectory m
     , MonadDocker m
     , MonadDownloadFile m
     , MonadGit m
     , MonadLogger m
     , MonadReadFile m
     , MonadReader env m
     , MonadUnliftIO m
     , MonadWriteFile m
     )
  => [SomePath]
  -> m (Maybe (NonEmpty RestylerResult))
runRestylers argPaths = do
  expPaths <- expandSomePaths argPaths
  paths <- removeExcluded expPaths

  remoteFiles <- asks getRemoteFiles
  for_ remoteFiles $ \rf -> downloadFile rf.url rf.path

  copyFiles <- asks getCopyFiles

  logDebug
    $ "Paths"
    :# [ "pathsGiven" .= argPaths
       , "pathsExpanded" .= truncateList toFilePath 50 expPaths
       , "pathsExpandedIncluded" .= truncateList toFilePath 50 paths
       ]

  restylers <- getEnabledRestylers
  mResults <- withCodeVolume $ \vol -> do
    copyCodeFiles remoteFiles paths vol copyFiles
    withFilteredPaths restylers paths $ runRestyler vol
  mResetTo <- join <$> traverse checkForNoop mResults

  case mResetTo of
    Nothing -> pure mResults
    Just ref -> do
      logInfo "Restylers offset each other, resetting git state"
      Nothing <$ gitResetHard ref

removeExcluded
  :: (HasExclude env, MonadReader env m)
  => [Path Rel File]
  -> m [Path Rel File]
removeExcluded ps = do
  exclude <- asks getExclude
  pure $ filter (\path -> none (`matchPath` path) exclude) ps

-- | See if multiple restylers offset each other
--
-- Returns the git ref to reset to if they did.
checkForNoop :: MonadGit m => NonEmpty RestylerResult -> m (Maybe String)
checkForNoop results = runMaybeT $ do
  sha <- hoistMaybe result.sha
  let parentRef = sha <> "^"
  changed <- lift $ gitDiffNameOnly $ Just parentRef
  parentRef <$ guard (null changed)
 where
  result = head results

-- | Run each @'Restyler'@ with appropriate paths out of the given set
--
-- Input is expected to be files (not directories), filtered for existence, and
-- processed through global @exclude@ already. This is extracted for specific
-- testing of Restyler @include@ and @intepreter@ configuration handling.
withFilteredPaths
  :: ( HasCallStack
     , MonadDirectory m
     , MonadLogger m
     , MonadReadFile m
     , MonadUnliftIO m
     )
  => [Restyler]
  -> [Path Rel File]
  -> (Restyler -> [Path Rel File] -> m (Maybe a))
  -> m (Maybe (NonEmpty a))
withFilteredPaths restylers paths run = do
  withInterpreters <- traverse addExecutableInterpreter paths

  mas <- for restylers $ \r -> do
    filtered <- (`mapMaybeM` withInterpreters) $ \(path, mInterpreter) -> do
      let
        matched = fromMaybe False $ do
          interpreter <- mInterpreter
          pure $ interpreter `elem` rInterpreters r
        includes =
          if matched
            then explicit (toFilePath path) : rInclude r
            else rInclude r
        included = includePath includes $ toFilePath path

      logTrace
        $ "Matching path"
        :# [ "name" .= rName r
           , "path" .= path
           , "matched" .= matched
           , "includes" .= includes
           , "included" .= included
           , "interpreter" .= mInterpreter
           ]

      pure $ if included then Just path else Nothing

    logDebug $ "Matched paths" :# ["name" .= rName r, "filtered" .= filtered]

    run r filtered

  pure $ nonEmpty $ catMaybes mas

addExecutableInterpreter
  :: (MonadDirectory m, MonadReadFile m, MonadUnliftIO m)
  => Path Rel File
  -> m (Path Rel File, Maybe Interpreter)
addExecutableInterpreter path = suppressWith (path, Nothing) $ do
  isExec <- isFileExecutable path

  (path,)
    <$> if isExec
      then readInterpreter <$> readFile path
      else pure Nothing

-- | Run a @'Restyler'@ and get the result (i.e. commit changes)
runRestyler
  :: ( HasCallStack
     , HasCommitTemplate env
     , HasDryRun env
     , HasImageCleanup env
     , HasNoCommit env
     , HasNoPull env
     , HasRestrictions env
     , MonadDirectory m
     , MonadDocker m
     , MonadGit m
     , MonadLogger m
     , MonadReadFile m
     , MonadReader env m
     , MonadUnliftIO m
     , MonadWriteFile m
     )
  => CodeVolume
  -> Restyler
  -> [Path Rel File]
  -> m (Maybe RestylerResult)
runRestyler vol r = \case
  [] -> pure Nothing
  paths -> do
    runRestyler_ vol r paths

    isGit <- isGitRepository

    if isGit
      then getRestylerResult paths r
      else do
        Nothing <$ logWarn "Unable to determine Restyler result (not a git repository)"

-- | Run a @'Restyler'@ (don't commit anything)
runRestyler_
  :: ( HasCallStack
     , HasDryRun env
     , HasImageCleanup env
     , HasNoPull env
     , HasRestrictions env
     , MonadDirectory m
     , MonadDocker m
     , MonadLogger m
     , MonadReadFile m
     , MonadReader env m
     , MonadUnliftIO m
     , MonadWriteFile m
     )
  => CodeVolume
  -> Restyler
  -> [Path Rel File]
  -> m ()
runRestyler_ _ _ [] = pure ()
runRestyler_ vol r paths = case rDelimiters r of
  Nothing -> run paths
  Just ds -> restyleDelimited ds run paths
 where
  run ps = do
    noPull <- asks $ (||) <$> getNoPull <*> getDryRun
    unless noPull $ dockerPullRestyler r
    dockerWithImageRm r
      $ traverse_ (dockerRunRestyler vol r)
      $ withProgress
      $ getDockerRunStyles r ps

data WithProgress a = WithProgress
  { pItem :: a
  , pIndex :: Natural
  , pTotal :: Natural
  }

withProgress :: [a] -> [WithProgress a]
withProgress xs = zipWith toWithProgress [1 ..] xs
 where
  toWithProgress n x =
    WithProgress
      { pItem = x
      , pIndex = n
      , pTotal = total
      }

  total = genericLength xs

data DockerRunStyle
  = DockerRunPathToStdout (Path Rel File)
  | DockerRunPathsOverwrite Bool [Path Rel File]
  | DockerRunPathOverwrite Bool (Path Rel File)
  deriving stock (Show)

getDockerRunStyles :: Restyler -> [Path Rel File] -> [DockerRunStyle]
getDockerRunStyles Restyler {..} paths = case rRunStyle of
  RestylerRunStylePathToStdout -> map DockerRunPathToStdout paths
  RestylerRunStylePathsOverwrite -> [DockerRunPathsOverwrite False paths]
  RestylerRunStylePathsOverwriteSep -> [DockerRunPathsOverwrite True paths]
  RestylerRunStylePathOverwrite -> map (DockerRunPathOverwrite False) paths
  RestylerRunStylePathOverwriteSep -> map (DockerRunPathOverwrite True) paths

dockerPullRestyler
  :: (HasCallStack, MonadDocker m, MonadIO m, MonadLogger m) => Restyler -> m ()
dockerPullRestyler r@Restyler {..} = do
  ec <- retrying policy shouldRetry $ \_ -> dockerPull rImage

  case ec of
    ExitSuccess -> pure ()
    ExitFailure i -> throw $ RestylerPullFailure r i
 where
  policy :: Monad m => RetryPolicyM m
  policy = exponentialBackoff (1 * 1000000) <> limitRetries retryLimit

  shouldRetry :: MonadLogger m => RetryStatus -> ExitCode -> m Bool
  shouldRetry status = \case
    ExitSuccess -> pure False
    ExitFailure i -> do
      when (rsIterNumber status > 0) $ do
        logWarn
          $ "Retrying docker-pull"
          :# [ "attempt" .= rsIterNumber status
             , "limit" .= retryLimit
             , "exitCode" .= i
             ]

      pure True

  retryLimit :: Int
  retryLimit = 5

dockerRunRestyler
  :: ( HasCallStack
     , HasDryRun env
     , HasRestrictions env
     , MonadDocker m
     , MonadLogger m
     , MonadReader env m
     , MonadUnliftIO m
     , MonadWriteFile m
     )
  => CodeVolume
  -> Restyler
  -> WithProgress DockerRunStyle
  -> m ()
dockerRunRestyler vol r@Restyler {..} WithProgress {..} = do
  restrictions <- asks getRestrictions
  dryRun <- asks getDryRun

  let
    cName = "restyler-" <> rName

    args =
      restrictionOptions restrictions
        <> ["--name", cName]
        <> ["--pull", "never"]
        <> ["--volume", vol.name.unwrap <> ":/code", rImage]
        <> nub (rCommand <> rArguments)

    copyRestyledPaths = traverse_ $ \path ->
      dockerCp (cName <> ":" <> toFilePath ([absdir|/code|] </> path))
        $ toFilePath path

    progressSuffix :: Text
    progressSuffix
      | pTotal > 1 = " (" <> pack (show pIndex) <> " of " <> pack (show pTotal) <> ")"
      | otherwise = ""

    logRunningOn =
      logInfo
        . (:# [])
        . (((if dryRun then "Would run " else "Running ") <> pack rName <> " on ") <>)
        . (<> progressSuffix)
        . \case
          [] -> "no paths" -- "impossible"
          [path] -> pack (toFilePath path)
          paths -> show (length paths) <> " paths"

    unlessDryRun f
      | dryRun = pure ExitSuccess
      | otherwise = f

    withDockerRm = (`finally` dockerRm cName)

  ec <- case pItem of
    DockerRunPathToStdout path -> do
      logRunningOn [path]
      unlessDryRun $ withDockerRm $ do
        (ec, out) <- dockerRunStdout $ args <> [prefix path]
        ec <$ writeFile path (fixNewline out)
    DockerRunPathsOverwrite sep paths -> do
      logRunningOn paths
      unlessDryRun $ withDockerRm $ do
        ec <- dockerRun $ args <> ["--" | sep] <> map prefix paths
        ec <$ copyRestyledPaths paths
    DockerRunPathOverwrite sep path -> do
      logRunningOn [path]
      unlessDryRun $ withDockerRm $ do
        ec <- dockerRun $ args <> ["--" | sep] <> [prefix path]
        ec <$ copyRestyledPaths [path]

  case ec of
    ExitSuccess -> pure ()
    ExitFailure 137 -> throw $ RestylerOutOfMemory r
    ExitFailure 127 -> throw $ RestylerCommandNotFound r
    ExitFailure i -> throw $ RestylerExitFailure r i
 where
  -- Some restylers (at least astyle) actually require an explicit ./ prefix for
  -- relative path arguments. It shouldn't be harmful to those that don't, so we
  -- do it all the time.
  prefix :: Path b File -> FilePath
  prefix = ("./" <>) . toFilePath

fixNewline :: Text -> Text
fixNewline = (<> "\n") . T.dropWhileEnd (== '\n')

-- | Remove the Restyler image after the given action
--
-- Our integration tests run every restyler we support in a space-restricted
-- environment. This switch triggers removal of each image after running it,
-- to avoid out-of-space errors.
dockerWithImageRm
  :: ( HasCallStack
     , HasDryRun env
     , HasImageCleanup env
     , MonadDocker m
     , MonadLogger m
     , MonadReader env m
     , MonadUnliftIO m
     )
  => Restyler
  -> m a
  -> m a
dockerWithImageRm r f = do
  imageCleanup <- asks $ (&&) <$> getImageCleanup <*> (not <$> getDryRun)
  if imageCleanup
    then finally f $ suppressWarn $ dockerImageRm $ rImage r
    else f

truncateList :: (a -> String) -> Int -> [a] -> [String]
truncateList f n ps
  | len > n = map f (take n ps) <> ["And " <> show (len - n) <> " more..."]
  | otherwise = map f ps
 where
  len = length ps
