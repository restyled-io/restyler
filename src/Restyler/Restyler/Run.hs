{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Restyler.Restyler.Run
  ( runRestylers

    -- * Errors
  , RestylerExitFailure (..)
  , RestylerOutOfMemory (..)
  , RestylerCommandNotFound (..)
  , TooManyChangedPaths (..)

    -- * Exported for testing only
  , runRestyler
  , runRestyler_
  , withFilteredPaths
  , findFiles
  ) where

import Restyler.Prelude

import Data.List (nub)
import Data.Text qualified as T
import Restyler.AnnotatedException (throw)
import Restyler.App.Class
import Restyler.Config
import Restyler.Config.ChangedPaths
import Restyler.Config.Glob (match)
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Delimited
import Restyler.Git
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.RemoteFile (downloadRemoteFile)
import Restyler.Restrictions
import Restyler.Restyler
import Restyler.RestylerResult
import Restyler.Wiki qualified as Wiki
import System.FilePath ((</>))
import UnliftIO.Exception (handleAny, tryAny)

data RestylerExitFailure = RestylerExitFailure Restyler Int
  deriving stock (Show, Eq)

instance Exception RestylerExitFailure where
  displayException (RestylerExitFailure Restyler {..} ec) =
    mconcat
      [ "Restyler " <> rName <> " exited non-zero (" <> show @String ec <> ")"
      , "\n  Error information may be present in debug messages printed above"
      , "\n"
      , "\n  Help:"
      , concatMap ("\n    " <>) rDocumentation
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

data TooManyChangedPaths = TooManyChangedPaths Natural Natural
  deriving stock (Show, Eq)

instance Exception TooManyChangedPaths where
  displayException (TooManyChangedPaths lenPaths maxPaths) =
    "Number of changed paths ("
      <> show lenPaths
      <> ") is greater than configured maximum ("
      <> show maxPaths
      <> ")"
      <> "\n  "
      <> unpack (Wiki.commonError "Restyle Error#too-many-changed-paths")

-- | Runs the configured @'Restyler'@s for the files and reports results
runRestylers
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadProcess m
     , MonadGit m
     , MonadDownloadFile m
     , MonadReader env m
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasRestrictions env
     , HasCallStack
     )
  => Config
  -> [FilePath]
  -> m [RestylerResult]
runRestylers config@Config {..} allPaths = do
  paths <- findFiles $ filter included allPaths

  logDebug $ "" :# ["restylers" .= map rName restylers]
  logDebug $ "" :# ["paths" .= paths]

  let
    lenPaths = genericLength paths
    maxPaths = cChangedPaths.maximum

  if lenPaths > maxPaths
    then case cChangedPaths.outcome of
      MaximumChangedPathsOutcomeSkip -> do
        logWarn
          $ "Number of changed paths is greater than configured maximum"
          :# ["paths" .= lenPaths, "maximum" .= maxPaths]
        pure []
      MaximumChangedPathsOutcomeError ->
        throw $ TooManyChangedPaths lenPaths maxPaths
    else do
      traverse_ downloadRemoteFile cRemoteFiles
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
  -> (Restyler -> [FilePath] -> m a)
  -> m [a]
withFilteredPaths restylers paths run = do
  withInterpreters <- traverse addExecutableInterpreter paths

  for restylers $ \r -> do
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

      logDebug
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

-- | TODO: warnIgnoreWith
addExecutableInterpreter
  :: (MonadUnliftIO m, MonadLogger m, MonadSystem m)
  => FilePath
  -> m (FilePath, Maybe Interpreter)
addExecutableInterpreter path = handleAny (const $ pure (path, Nothing)) $ do
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
     , MonadProcess m
     , MonadGit m
     , MonadReader env m
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasRestrictions env
     , HasCallStack
     )
  => Config
  -> Restyler
  -> [FilePath]
  -> m RestylerResult
runRestyler _ r [] = pure $ noPathsRestylerResult r
runRestyler config r paths = do
  runRestyler_ r paths
  getRestylerResult config r

-- | Run a @'Restyler'@ (don't commit anything)
runRestyler_
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadProcess m
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
  :: (MonadLogger m, MonadProcess m, HasCallStack) => Restyler -> m ()
dockerPullRestyler Restyler {..} = do
  logInfo $ "Pulling Restyler" :# ["image" .= rImage]
  callProcess "docker" ["pull", "--quiet", rImage]

dockerRunRestyler
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadProcess m
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
      ["run", "--rm"]
        <> restrictionOptions restrictions
        <> ["--volume", cwd <> ":/code", rImage]
        <> nub (rCommand <> rArguments)

    progress :: Text
    progress = pack (show pIndex) <> " of " <> pack (show pTotal)

    -- Our integration tests run every restyler we support in a space-restricted
    -- environment. This switch triggers removal of each image after running it,
    -- to avoid out-of-space errors.
    withImageCleanup f = if imageCleanup then f `finally` cleanupImage else f

  logInfo
    $ "Restyling"
    :# [ "restyler" .= rName
       , "run" .= progress
       , "style" .= rRunStyle
       ]

  ec <- withImageCleanup $ case pItem of
    DockerRunPathToStdout path -> do
      (ec, out) <- readProcessExitCode "docker" (args <> [prefix path])
      ec <$ writeFile path (fixNewline $ pack out)
    DockerRunPathsOverwrite sep paths -> do
      callProcessExitCode "docker" $ args <> ["--" | sep] <> map prefix paths
    DockerRunPathOverwrite sep path -> do
      callProcessExitCode "docker" $ args <> ["--" | sep] <> [prefix path]

  case ec of
    ExitSuccess -> pure ()
    ExitFailure 137 -> throw $ RestylerOutOfMemory r
    ExitFailure 127 -> throw $ RestylerCommandNotFound r
    ExitFailure i -> throw $ RestylerExitFailure r i
 where
  prefix p
    | "./" `isPrefixOf` p = p
    | otherwise = "./" <> p

  cleanupImage = do
    eec <- tryAny $ callProcessExitCode "docker" ["image", "rm", "--force", rImage]
    case eec of
      Left ex ->
        logWarn
          $ "Exception removing Restyler image"
          :# ["exception" .= displayException ex]
      Right ExitSuccess ->
        logInfo "Removed Restyler image"
      Right (ExitFailure i) ->
        logWarn
          $ "Error removing Restyler image"
          :# ["status" .= i]

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
