{-# LANGUAGE TupleSections #-}

module Restyler.Restyler.Run
  ( runRestylers
  , runRestylers_

    -- * Errors
  , RestylerExitFailure (..)
  , RestylerOutOfMemory (..)
  , TooManyChangedPaths (..)

    -- * Exported for testing only
  , runRestyler
  , runRestyler_
  , withFilteredPaths
  , findFiles
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.List (nub)
import qualified Data.Text as T
import Restyler.App.Class
import Restyler.Config
import Restyler.Config.ChangedPaths
import Restyler.Config.Glob (match)
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Delimited
import Restyler.Git
import Restyler.Options
import Restyler.RemoteFile (downloadRemoteFile)
import Restyler.Restrictions
import Restyler.Restyler
import Restyler.RestylerResult
import qualified Restyler.Wiki as Wiki
import System.FilePath ((</>))

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
      , "\n  " <> unpack (Wiki.commonError "Restyle Error 137")
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
     , HasConfig env
     , HasOptions env
     )
  => Config
  -> [FilePath]
  -> m [RestylerResult]
runRestylers = runRestylersWith runRestyler

-- | @'runRestylers'@, but without committing or reporting results
runRestylers_
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadProcess m
     , MonadDownloadFile m
     , MonadReader env m
     , HasOptions env
     )
  => Config
  -> [FilePath]
  -> m ()
runRestylers_ config = void . runRestylersWith runRestyler_ config

runRestylersWith
  :: (MonadUnliftIO m, MonadLogger m, MonadSystem m, MonadDownloadFile m)
  => (Restyler -> [FilePath] -> m a)
  -> Config
  -> [FilePath]
  -> m [a]
runRestylersWith run Config {..} allPaths = do
  paths <- findFiles $ filter included allPaths

  logDebug $ "" :# ["restylers" .= map rName restylers]
  logDebug $ "" :# ["paths" .= paths]

  let
    lenPaths = genericLength paths
    maxPaths = cpcMaximum cChangedPaths

  if lenPaths > maxPaths
    then case cpcOutcome cChangedPaths of
      MaximumChangedPathsOutcomeSkip -> do
        logWarn $
          "Number of changed paths is greater than configured maximum"
            :# ["paths" .= lenPaths, "maximum" .= maxPaths]
        pure []
      MaximumChangedPathsOutcomeError ->
        throwIO $ TooManyChangedPaths lenPaths maxPaths
    else do
      traverse_ downloadRemoteFile cRemoteFiles
      withFilteredPaths restylers paths run
 where
  included path = none (`match` path) cExclude
  restylers = filter rEnabled cRestylers

-- | Run each @'Restyler'@ with appropriate paths out of the given set
--
-- Input is expected to be files (not directories), filtered for existence, and
-- processed through global @exclude@ already. This is extracted for specific
-- testing of Restyler @include@ and @intepreter@ configuration handling.
withFilteredPaths
  :: (MonadUnliftIO m, MonadLogger m, MonadSystem m)
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

      logDebug $
        "Matching paths"
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

addExecutableInterpreter
  :: (MonadUnliftIO m, MonadLogger m, MonadSystem m)
  => FilePath
  -> m (FilePath, Maybe Interpreter)
addExecutableInterpreter path = warnIgnoreWith (path, Nothing) $ do
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
     , HasConfig env
     , HasOptions env
     )
  => Restyler
  -> [FilePath]
  -> m RestylerResult
runRestyler r [] = pure $ noPathsRestylerResult r
runRestyler r paths = do
  runRestyler_ r paths
  getRestylerResult r

-- | Run a @'Restyler'@ (don't commit anything)
runRestyler_
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadProcess m
     , MonadReader env m
     , HasOptions env
     )
  => Restyler
  -> [FilePath]
  -> m ()
runRestyler_ _ [] = pure ()
runRestyler_ r paths = case rDelimiters r of
  Nothing -> runRestyler' r paths
  Just ds -> restyleDelimited ds (runRestyler' r) paths

runRestyler'
  :: ( MonadIO m
     , MonadLogger m
     , MonadSystem m
     , MonadProcess m
     , MonadReader env m
     , HasOptions env
     )
  => Restyler
  -> [FilePath]
  -> m ()
runRestyler' r@Restyler {..} paths
  | fromMaybe False rRunAsFilter =
      traverse_ (dockerRunRestyler r . DockerRunFilter) paths
  | rSupportsMultiplePaths =
      dockerRunRestyler r $ DockerRunMany rSupportsArgSep paths
  | otherwise =
      traverse_ (dockerRunRestyler r . DockerRunSingle rSupportsArgSep) paths

data DockerRunStyle
  = DockerRunFilter FilePath
  | DockerRunMany Bool [FilePath]
  | DockerRunSingle Bool FilePath
  deriving stock (Show)

dockerRunStyleToText :: DockerRunStyle -> Text
dockerRunStyleToText = \case
  DockerRunFilter {} -> "as filter"
  DockerRunMany sep _paths ->
    if sep
      then "many files, with separator"
      else "many files, without separator"
  DockerRunSingle sep _path ->
    if sep
      then "single file, with separator"
      else "single file, without separator"

instance ToJSON DockerRunStyle where
  toJSON = toJSON . dockerRunStyleToText
  toEncoding = toEncoding . dockerRunStyleToText

dockerRunRestyler
  :: ( MonadIO m
     , MonadLogger m
     , MonadSystem m
     , MonadProcess m
     , MonadReader env m
     , HasOptions env
     )
  => Restyler
  -> DockerRunStyle
  -> m ()
dockerRunRestyler r@Restyler {..} style = do
  cwd <- getHostDirectory
  restrictions <- oRestrictions <$> view optionsL

  let args =
        ["run", "--rm"]
          <> restrictionOptions restrictions
          <> ["--volume", cwd <> ":/code", rImage]
          <> nub (rCommand <> rArguments)

  logInfo $
    "Restyling"
      :# [ "restyler" .= rName
         , "style" .= style
         ]

  ec <- case style of
    DockerRunFilter path -> do
      (ec, out) <- readProcessExitCode "docker" (args <> [prefix path])
      ec <$ writeFile path (fixNewline $ pack out)
    DockerRunMany sep paths -> do
      callProcessExitCode "docker" $ args <> ["--" | sep] <> map prefix paths
    DockerRunSingle sep path -> do
      callProcessExitCode "docker" $ args <> ["--" | sep] <> [prefix path]

  case ec of
    ExitSuccess -> pure ()
    ExitFailure 137 -> throwIO $ RestylerOutOfMemory r
    ExitFailure i -> throwIO $ RestylerExitFailure r i
 where
  prefix p
    | "./" `isPrefixOf` p = p
    | otherwise = "./" <> p

fixNewline :: Text -> Text
fixNewline = (<> "\n") . T.dropWhileEnd (== '\n')

getHostDirectory
  :: (MonadSystem m, MonadReader env m, HasOptions env) => m FilePath
getHostDirectory = do
  mHostDirectory <- oHostDirectory <$> view optionsL
  maybe getCurrentDirectory pure mHostDirectory

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
