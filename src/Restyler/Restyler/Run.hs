{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Restyler.Restyler.Run
  ( runRestylers

    -- * Errors
  , RestylerPullFailure (..)
  , RestylerExitFailure (..)
  , RestylerOutOfMemory (..)
  , RestylerCommandNotFound (..)

    -- * Exported for testing only
  , runRestyler
  , runRestyler_
  , withFilteredPaths
  , findFiles
  ) where

import Restyler.Prelude

import Data.List (nub)
import Data.Text qualified as T
import Restyler.AnnotatedException
import Restyler.App.Class
import Restyler.Config
import Restyler.Config.Glob (match)
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Config.RemoteFile
import Restyler.Delimited
import Restyler.Docker
import Restyler.Git
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.NoCommit
import Restyler.Restrictions
import Restyler.Restyler
import Restyler.RestylerResult
import Restyler.Wiki qualified as Wiki
import System.FilePath ((</>))

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
      , "\n"
      , "\nSee " <> unpack (Wiki.commonError "Restyle Error 137") <> " for more details"
      ]

newtype RestylerCommandNotFound = RestylerCommandNotFound Restyler
  deriving stock (Show, Eq)

instance Exception RestylerCommandNotFound where
  displayException (RestylerCommandNotFound Restyler {..}) =
    mconcat
      [ "Restyler " <> rName <> " has an invalid command (exit code 127)"
      , "\n"
      , "You may need to adjust restylers[" <> rName <> "].command"
      , "\n"
      , "\nSee " <> unpack (Wiki.commonError "Restyle Error 127") <> " for more details"
      ]

-- | Runs the configured @'Restyler'@s for the files and reports results
runRestylers
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadGit m
     , MonadDocker m
     , MonadDownloadFile m
     , MonadReader env m
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasNoCommitOption env
     , HasRestrictions env
     , HasCallStack
     )
  => Config
  -> [FilePath]
  -> m (Maybe (NonEmpty RestylerResult))
runRestylers config@Config {..} allPaths = do
  paths <- findFiles $ filter included allPaths
  for_ cRemoteFiles $ \rf -> downloadFile rf.url rf.path
  withFilteredPaths restylers paths $ runRestyler config
 where
  included path = none (`match` path) cExclude
  restylers = filter rEnabled cRestylers

-- | Run each @'Restyler'@ with appropriate paths out of the given set
--
-- Input is expected to be files (not directories), filtered for existence, and
-- processed through global @exclude@ already. This is extracted for specific
-- testing of Restyler @include@ and @intepreter@ configuration handling.
withFilteredPaths
  :: (MonadUnliftIO m, MonadLogger m, MonadSystem m, HasCallStack)
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
        $ "Matching paths"
        :# [ "name" .= rName r
           , "path" .= path
           , "matched" .= matched
           , "includes" .= includes
           , "included" .= included
           , "interpreter" .= mInterpreter
           -- , "filtered" .= filtered
           ]

      pure $ if included then Just path else Nothing

    run r filtered

  pure $ nonEmpty $ catMaybes mas

addExecutableInterpreter
  :: (MonadUnliftIO m, MonadLogger m, MonadSystem m)
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
     , MonadSystem m
     , MonadGit m
     , MonadDocker m
     , MonadReader env m
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasNoCommitOption env
     , HasRestrictions env
     , HasCallStack
     )
  => Config
  -> Restyler
  -> [FilePath]
  -> m (Maybe RestylerResult)
runRestyler config r = \case
  [] -> pure Nothing
  paths -> do
    runRestyler_ r paths
    getRestylerResult config r

-- | Run a @'Restyler'@ (don't commit anything)
runRestyler_
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadDocker m
     , MonadReader env m
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
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
    dockerPullRestyler r
    traverse_ (dockerRunRestyler r) $ withProgress $ getDockerRunStyles r ps

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
     , MonadSystem m
     , MonadDocker m
     , MonadReader env m
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasRestrictions env
     , HasCallStack
     )
  => Restyler
  -> WithProgress DockerRunStyle
  -> m ()
dockerRunRestyler r@Restyler {..} WithProgress {..} = do
  cwd <- getHostDirectory
  imageCleanup <- getImageCleanup
  restrictions <- asks getRestrictions

  let
    args =
      restrictionOptions restrictions
        <> ["--volume", cwd <> ":/code", rImage]
        <> nub (rCommand <> rArguments)

    progressSuffix :: Text
    progressSuffix
      | pTotal > 1 = " (" <> pack (show pIndex) <> " of " <> pack (show pTotal) <> ")"
      | otherwise = ""

    -- Our integration tests run every restyler we support in a space-restricted
    -- environment. This switch triggers removal of each image after running it,
    -- to avoid out-of-space errors.
    withImageCleanup f =
      if imageCleanup
        then f `finally` suppressWarn (dockerImageRm rImage)
        else f

    logRunningOn =
      logInfo
        . (:# [])
        . (("Running " <> pack rName <> " on ") <>)
        . (<> progressSuffix)
        . \case
          [] -> "no paths" -- "impossible"
          [path] -> pack path
          paths -> show (length paths) <> " paths"

  ec <- withImageCleanup $ case pItem of
    DockerRunPathToStdout path -> do
      logRunningOn [path]
      (ec, out) <- dockerRunStdout $ args <> [prefix path]
      ec <$ writeFile path (fixNewline out)
    DockerRunPathsOverwrite sep paths -> do
      logRunningOn paths
      dockerRun $ args <> ["--" | sep] <> map prefix paths
    DockerRunPathOverwrite sep path -> do
      logRunningOn [path]
      dockerRun $ args <> ["--" | sep] <> [prefix path]

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

-- | Expand directory arguments and filter to only existing paths
--
-- The existence filtering is important for normal Restyling, where we may get
-- path arguments of removed files in the PR. The expansion is important for
-- @restyle-path@, where we may be given directories as arguments.
findFiles :: MonadSystem m => [FilePath] -> m [FilePath]
findFiles = fmap concat . traverse go
 where
  go :: MonadSystem m => FilePath -> m [FilePath]
  go parent = do
    isDirectory <- doesDirectoryExist parent

    if isDirectory
      then do
        files <- listDirectory parent
        findFiles $ map (parent </>) files
      else fmap maybeToList $ runMaybeT $ do
        guardM $ lift $ doesFileExist parent
        guardM $ lift $ not <$> isFileSymbolicLink parent
        pure parent
