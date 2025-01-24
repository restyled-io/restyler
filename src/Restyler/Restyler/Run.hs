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
  , findFiles
  ) where

import Restyler.Prelude

import Data.List (nub)
import Data.Text qualified as T
import Restyler.AnnotatedException
import Restyler.Config
import Restyler.Config.Glob (match)
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Delimited
import Restyler.Monad.Directory
import Restyler.Monad.Docker
import Restyler.Monad.DownloadFile
import Restyler.Monad.Git
import Restyler.Monad.ReadFile
import Restyler.Monad.WriteFile
import Restyler.Restyler
import Restyler.RestylerResult
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

data RestylerPullFailure = RestylerPullFailure Restyler Int
  deriving stock (Show, Eq)

instance Exception RestylerPullFailure where
  displayException (RestylerPullFailure Restyler {..} ec) =
    mconcat
      [ "Unable to pull: " <> rImage <> " (exit " <> show @String ec <> ")"
      , "\nThe source of the error may be visible in debug logging"
      ]

data RestylerExitFailure = RestylerExitFailure Restyler Int
  deriving stock (Show, Eq)

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
  deriving stock (Show, Eq)

instance Exception RestylerOutOfMemory where
  displayException (RestylerOutOfMemory Restyler {..}) =
    mconcat
      [ "Restyler " <> rName <> " used too much memory (exit code 137)"
      ]

newtype RestylerCommandNotFound = RestylerCommandNotFound Restyler
  deriving stock (Show, Eq)

instance Exception RestylerCommandNotFound where
  displayException (RestylerCommandNotFound Restyler {..}) =
    mconcat
      [ "Restyler " <> rName <> " has an invalid command (exit code 127)"
      , "\n"
      , "You may need to adjust restylers[" <> rName <> "].command"
      ]

-- | Runs the configured @'Restyler'@s for the files and reports results
runRestylers
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadDirectory m
     , MonadReadFile m
     , MonadWriteFile m
     , MonadGit m
     , MonadDocker m
     , MonadDownloadFile m
     , MonadReader env m
     , HasCommitTemplate env
     , HasDryRun env
     , HasExclude env
     , HasHostDirectory env
     , HasImageCleanup env
     , HasManifest env
     , HasNoCommit env
     , HasNoPull env
     , HasRemoteFiles env
     , HasRestrictions env
     , HasRestylerOverrides env
     , HasRestylersVersion env
     , HasCallStack
     )
  => [FilePath]
  -> m (Maybe (NonEmpty RestylerResult))
runRestylers argPaths = do
  allPaths <- removeExcluded $ map FilePath.normalise argPaths
  expPaths <- findFiles allPaths
  paths <- removeExcluded expPaths

  logDebug
    $ "Paths"
    :# [ "pathsGiven" .= argPaths
       , "pathsGivenIncluded" .= allPaths
       , "pathsExpanded" .= truncatePaths 50 expPaths
       , "pathsExpandedIncluded" .= truncatePaths 50 paths
       ]

  remoteFiles <- asks getRemoteFiles
  for_ remoteFiles $ \rf -> downloadFile rf.url rf.path

  restylers <- getEnabledRestylers
  mResults <- withFilteredPaths restylers paths runRestyler
  mResetTo <- join <$> traverse checkForNoop mResults

  case mResetTo of
    Nothing -> pure mResults
    Just ref -> do
      logInfo "Restylers offset each other, resetting git state"
      Nothing <$ gitResetHard ref

removeExcluded
  :: (MonadReader env m, HasExclude env)
  => [FilePath]
  -> m [FilePath]
removeExcluded ps = do
  exclude <- asks getExclude
  pure $ filter (\path -> none (`match` path) exclude) ps

-- | See if multiple restylers offset each other
--
-- Returns the git ref to reset to if they did.
checkForNoop :: MonadGit m => NonEmpty RestylerResult -> m (Maybe String)
checkForNoop results = runMaybeT $ do
  sha <- hoistMaybe result.sha
  let parent = sha <> "^"
  changed <- lift $ gitDiffNameOnly $ Just parent
  parent <$ guard (null changed)
 where
  result = head results

-- | Run each @'Restyler'@ with appropriate paths out of the given set
--
-- Input is expected to be files (not directories), filtered for existence, and
-- processed through global @exclude@ already. This is extracted for specific
-- testing of Restyler @include@ and @intepreter@ configuration handling.
withFilteredPaths
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadDirectory m
     , MonadReadFile m
     , HasCallStack
     )
  => [Restyler]
  -> [FilePath]
  -> (Restyler -> [FilePath] -> m (Maybe a))
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
            then explicit path : rInclude r
            else rInclude r
        included = includePath includes path

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
  :: (MonadUnliftIO m, MonadLogger m, MonadDirectory m, MonadReadFile m)
  => FilePath
  -> m (FilePath, Maybe Interpreter)
addExecutableInterpreter path = suppressWith (path, Nothing) $ do
  isExec <- isFileExecutable path

  (path,)
    <$> if isExec
      then readInterpreter <$> readFile path
      else pure Nothing

-- | Run a @'Restyler'@ and get the result (i.e. commit changes)
runRestyler
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadDirectory m
     , MonadReadFile m
     , MonadWriteFile m
     , MonadGit m
     , MonadDocker m
     , MonadReader env m
     , HasCommitTemplate env
     , HasDryRun env
     , HasHostDirectory env
     , HasImageCleanup env
     , HasNoCommit env
     , HasNoPull env
     , HasRestrictions env
     , HasCallStack
     )
  => Restyler
  -> [FilePath]
  -> m (Maybe RestylerResult)
runRestyler r = \case
  [] -> pure Nothing
  paths -> do
    runRestyler_ r paths

    isGit <- isGitRepository

    if isGit
      then getRestylerResult paths r
      else do
        Nothing <$ logWarn "Unable to determine Restyler result (not a git repository)"

-- | Run a @'Restyler'@ (don't commit anything)
runRestyler_
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadDirectory m
     , MonadReadFile m
     , MonadWriteFile m
     , MonadDocker m
     , MonadReader env m
     , HasDryRun env
     , HasHostDirectory env
     , HasImageCleanup env
     , HasNoPull env
     , HasRestrictions env
     , HasCallStack
     )
  => Restyler
  -> [FilePath]
  -> m ()
runRestyler_ _ [] = pure ()
runRestyler_ r paths = case rDelimiters r of
  Nothing -> run paths
  Just ds -> restyleDelimited ds run paths
 where
  run ps = do
    noPull <- asks $ (||) <$> getNoPull <*> getDryRun
    unless noPull $ dockerPullRestyler r
    dockerWithImageRm r
      $ traverse_ (dockerRunRestyler r)
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
  = DockerRunPathToStdout FilePath
  | DockerRunPathsOverwrite Bool [FilePath]
  | DockerRunPathOverwrite Bool FilePath
  deriving stock (Show)

getDockerRunStyles :: Restyler -> [FilePath] -> [DockerRunStyle]
getDockerRunStyles Restyler {..} paths = case rRunStyle of
  RestylerRunStylePathToStdout -> map DockerRunPathToStdout paths
  RestylerRunStylePathsOverwrite -> [DockerRunPathsOverwrite False paths]
  RestylerRunStylePathsOverwriteSep -> [DockerRunPathsOverwrite True paths]
  RestylerRunStylePathOverwrite -> map (DockerRunPathOverwrite False) paths
  RestylerRunStylePathOverwriteSep -> map (DockerRunPathOverwrite True) paths

dockerPullRestyler
  :: (MonadIO m, MonadDocker m, HasCallStack) => Restyler -> m ()
dockerPullRestyler r@Restyler {..} = do
  ec <- dockerPull rImage
  case ec of
    ExitSuccess -> pure ()
    ExitFailure i -> throw $ RestylerPullFailure r i

dockerRunRestyler
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadWriteFile m
     , MonadDocker m
     , MonadReader env m
     , HasDryRun env
     , HasHostDirectory env
     , HasRestrictions env
     , HasCallStack
     )
  => Restyler
  -> WithProgress DockerRunStyle
  -> m ()
dockerRunRestyler r@Restyler {..} WithProgress {..} = do
  cwd <- asks getHostDirectory
  restrictions <- asks getRestrictions
  dryRun <- asks getDryRun

  let
    args =
      restrictionOptions restrictions
        <> ["--pull", "never"]
        <> ["--volume", toFilePath cwd <> ":/code", rImage]
        <> nub (rCommand <> rArguments)

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
          [path] -> pack path
          paths -> show (length paths) <> " paths"

    unlessDryRun f
      | dryRun = pure ExitSuccess
      | otherwise = f

  ec <- case pItem of
    DockerRunPathToStdout path -> do
      logRunningOn [path]
      unlessDryRun $ do
        (ec, out) <- dockerRunStdout $ args <> [prefix path]
        ec <$ writeFile path (fixNewline out)
    DockerRunPathsOverwrite sep paths -> do
      logRunningOn paths
      unlessDryRun $ dockerRun $ args <> ["--" | sep] <> map prefix paths
    DockerRunPathOverwrite sep path -> do
      logRunningOn [path]
      unlessDryRun $ dockerRun $ args <> ["--" | sep] <> [prefix path]

  case ec of
    ExitSuccess -> pure ()
    ExitFailure 137 -> throw $ RestylerOutOfMemory r
    ExitFailure 127 -> throw $ RestylerCommandNotFound r
    ExitFailure i -> throw $ RestylerExitFailure r i
 where
  prefix p
    | "./" `isPrefixOf` p = p
    | otherwise = "./" <> p

fixNewline :: Text -> Text
fixNewline = (<> "\n") . T.dropWhileEnd (== '\n')

-- | Remove the Restyler image after the given action
--
-- Our integration tests run every restyler we support in a space-restricted
-- environment. This switch triggers removal of each image after running it,
-- to avoid out-of-space errors.
dockerWithImageRm
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadDocker m
     , MonadReader env m
     , HasDryRun env
     , HasImageCleanup env
     , HasCallStack
     )
  => Restyler
  -> m a
  -> m a
dockerWithImageRm r f = do
  imageCleanup <- asks $ (&&) <$> getImageCleanup <*> (not <$> getDryRun)
  if imageCleanup
    then finally f $ suppressWarn $ dockerImageRm $ rImage r
    else f

-- | Expand directory arguments and filter to only existing paths
--
-- The existence filtering is important for normal Restyling, where we may get
-- path arguments of removed files in the PR. The expansion is important for
-- @restyle-path@, where we may be given directories as arguments.
findFiles :: MonadDirectory m => [FilePath] -> m [FilePath]
findFiles = fmap concat . traverse go
 where
  go :: MonadDirectory m => FilePath -> m [FilePath]
  go parent = do
    isDirectory <- doesDirectoryExist parent

    if isDirectory
      then do
        files <- listDirectory parent
        findFiles $ map (parent </>) $ filter (not . isHidden) files
      else fmap maybeToList $ runMaybeT $ do
        guardM $ lift $ doesFileExist parent
        guardM $ lift $ not <$> pathIsSymbolicLink parent
        pure $ FilePath.normalise parent

isHidden :: FilePath -> Bool
isHidden path = "." `isPrefixOf` path && path `notElem` [".", ".."]

truncatePaths :: Int -> [FilePath] -> [String]
truncatePaths n ps
  | len > n = take n ps <> ["And " <> show (len - n) <> " more..."]
  | otherwise = ps
 where
  len = length ps
