

module Restyler.Restyler.Run
    ( runRestylers
    , runRestylers_

    -- * Exported for testing only
    , runRestyler
    , runRestyler_
    , withFilteredPaths
    , findFiles
    )
where

import Restyler.Prelude

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (nub)
import Restyler.App.Error
import Restyler.Capabilities.DownloadFile
import Restyler.Capabilities.Git
import Restyler.Capabilities.Hushed
import Restyler.Capabilities.Logger
import Restyler.Capabilities.Process
import Restyler.Capabilities.System
import Restyler.Config
import Restyler.Config.ChangedPaths
import Restyler.Config.Glob (match)
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Delimited
import Restyler.Options
import Restyler.RemoteFile (downloadRemoteFile)
import Restyler.Restyler
import Restyler.RestylerResult
import RIO.FilePath ((</>))

-- | Runs the configured @'Restyler'@s for the files and reports results
runRestylers
    :: ( MonadLogger m
       , MonadHushed m
       , MonadSystem m
       , MonadProcess m
       , MonadDownloadFile m
       , MonadGit m
       , MonadError AppError m
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
    :: ( MonadLogger m
       , MonadHushed m
       , MonadSystem m
       , MonadProcess m
       , MonadDownloadFile m
       , MonadError AppError m
       , MonadReader env m
       , HasOptions env
       )
    => Config
    -> [FilePath]
    -> m ()
runRestylers_ config = void . runRestylersWith runRestyler_ config

runRestylersWith
    :: ( MonadLogger m
       , MonadHushed m
       , MonadSystem m
       , MonadDownloadFile m
       , MonadError AppError m
       )
    => (Restyler -> [FilePath] -> m a)
    -> Config
    -> [FilePath]
    -> m [a]
runRestylersWith run Config {..} allPaths = do
    paths <- findFiles $ filter included allPaths

    logDebug $ "Restylers: " <> displayShow (map rName restylers)
    logDebug $ "Paths: " <> displayShow paths

    let lenPaths = genericLength paths
        maxPaths = cpcMaximum cChangedPaths
        maxPathsLogMessage =
            "Number of changed paths ("
                <> displayShow lenPaths
                <> ") is greater than configured maximum ("
                <> displayShow maxPaths
                <> ")"

    if lenPaths > maxPaths
        then case cpcOutcome cChangedPaths of
            MaximumChangedPathsOutcomeSkip -> [] <$ logWarn maxPathsLogMessage
            MaximumChangedPathsOutcomeError ->
                throwError $ RestyleError $ utf8BuilderToText maxPathsLogMessage
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
--
withFilteredPaths
    :: (MonadLogger m, MonadHushed m, MonadSystem m)
    => [Restyler]
    -> [FilePath]
    -> (Restyler -> [FilePath] -> m a)
    -> m [a]
withFilteredPaths restylers paths run = do
    withInterpreters <- traverse addExecutableInterpreter paths

    for restylers $ \r -> do
        logDebug $ "Matching paths for " <> fromString (rName r)
        filtered <- (`mapMaybeM` withInterpreters) $ \(path, mInterpreter) -> do
            let matched = fromMaybe False $ do
                    interpreter <- mInterpreter
                    pure $ interpreter `elem` rInterpreters r
                includes = if matched
                    then explicit path : rInclude r
                    else rInclude r
                included = includePath includes path

            for_ mInterpreter $ \interpreter ->
                logDebug
                    $ "Interpreter "
                    <> displayShow interpreter
                    <> ": "
                    <> (if matched then "matched" else "no match")

            logDebug
                $ displayShow includes
                <> " includes "
                <> displayShow path
                <> "? "
                <> displayShow included

            pure $ if included then Just path else Nothing

        logDebug $ "Filtered paths for: " <> displayShow filtered
        run r filtered

addExecutableInterpreter
    :: (MonadHushed m, MonadSystem m)
    => FilePath
    -> m (FilePath, Maybe Interpreter)
addExecutableInterpreter path = do
    mmInterpreter <- hushed $ do
        isExec <- isFileExecutable path
        if isExec then readInterpreter <$> readFile path else pure Nothing

    pure (path, join mmInterpreter)

-- | Run a @'Restyler'@ and get the result (i.e. commit changes)
runRestyler
    :: ( MonadLogger m
       , MonadSystem m
       , MonadProcess m
       , MonadGit m
       , MonadError AppError m
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
    :: ( MonadLogger m
       , MonadSystem m
       , MonadProcess m
       , MonadError AppError m
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
    :: ( MonadLogger m
       , MonadSystem m
       , MonadProcess m
       , MonadError AppError m
       , MonadReader env m
       , HasOptions env
       )
    => Restyler
    -> [FilePath]
    -> m ()
runRestyler' r@Restyler {..} paths = if rSupportsMultiplePaths
    then do
        logInfo
            $ "Restyling "
            <> displayShow paths
            <> " via "
            <> displayShow rName
        dockerRunRestyler r paths
    else for_ paths $ \path -> do
        logInfo
            $ "Restyling "
            <> displayShow path
            <> " via "
            <> displayShow rName
        dockerRunRestyler r [path]

dockerRunRestyler
    :: ( MonadLogger m
       , MonadSystem m
       , MonadProcess m
       , MonadError AppError m
       , MonadReader env m
       , HasOptions env
       )
    => Restyler
    -> [FilePath]
    -> m ()
dockerRunRestyler r@Restyler {..} paths = do
    cwd <- getHostDirectory
    unrestricted <- oUnrestricted <$> view optionsL
    ec <-
        callProcessExitCode "docker"
        $ ["run", "--rm", "--net", "none"]
        <> bool restrictions [] unrestricted
        <> ["--volume", cwd <> ":/code", rImage]
        <> nub (rCommand <> rArguments)
        <> [ "--" | rSupportsArgSep ]
        <> map ("./" <>) paths

    case ec of
        ExitSuccess -> pure ()
        ExitFailure s -> throwError $ RestylerExitFailure r s paths

restrictions :: [String]
restrictions = ["--cap-drop", "all", "--cpu-shares", "128", "--memory", "512m"]

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
--
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
