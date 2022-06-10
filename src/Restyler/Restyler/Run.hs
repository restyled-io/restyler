{-# LANGUAGE TupleSections #-}

module Restyler.Restyler.Run
    ( runRestylers
    , runRestylers_

    -- * Exported for testing only
    , runRestyler
    , runRestyler_
    , withFilteredPaths
    , findFiles
    ) where

import Restyler.Prelude

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (nub)
import RIO.FilePath ((</>))
import Restyler.App.Class
import Restyler.App.Error
import Restyler.Config
import Restyler.Config.ChangedPaths
import Restyler.Config.Glob (match)
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Delimited
import Restyler.Git
import Restyler.Options
import Restyler.RemoteFile (downloadRemoteFile)
import Restyler.Restyler
import Restyler.RestylerResult

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

    let lenPaths = genericLength paths
        maxPaths = cpcMaximum cChangedPaths

    if lenPaths > maxPaths
        then case cpcOutcome cChangedPaths of
            MaximumChangedPathsOutcomeSkip -> do
                logWarn
                    $ "Number of changed paths is greater than configured maximum"
                    :# ["paths" .= lenPaths, "maximum" .= maxPaths]
                pure []
            MaximumChangedPathsOutcomeError ->
                throwIO
                    $ RestyleError
                    $ "Number of changed paths ("
                    <> pack (show lenPaths)
                    <> ") is greater than configured maximum ("
                    <> pack (show maxPaths)
                    <> ")"
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
    :: (MonadUnliftIO m, MonadLogger m, MonadSystem m)
    => [Restyler]
    -> [FilePath]
    -> (Restyler -> [FilePath] -> m a)
    -> m [a]
withFilteredPaths restylers paths run = do
    withInterpreters <- traverse addExecutableInterpreter paths

    for restylers $ \r -> do
        filtered <- (`mapMaybeM` withInterpreters) $ \(path, mInterpreter) -> do
            let matched = fromMaybe False $ do
                    interpreter <- mInterpreter
                    pure $ interpreter `elem` rInterpreters r
                includes = if matched
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

addExecutableInterpreter
    :: (MonadUnliftIO m, MonadSystem m)
    => FilePath
    -> m (FilePath, Maybe Interpreter)
addExecutableInterpreter path = handleAny (const $ pure (path, Nothing)) $ do
    isExec <- isFileExecutable path

    (path, ) <$> if isExec
        then readInterpreter <$> readFile path
        else pure Nothing

-- | Run a @'Restyler'@ and get the result (i.e. commit changes)
runRestyler
    :: ( MonadIO m
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
runRestyler' r@Restyler {..} paths = if rSupportsMultiplePaths
    then do
        logInfo $ "Restyling" :# ["paths" .= paths, "restyler" .= rName]
        dockerRunRestyler r paths
    else for_ paths $ \path -> do
        logInfo $ "Restyling" :# ["paths" .= [path], "restyler" .= rName]
        dockerRunRestyler r [path]

dockerRunRestyler
    :: ( MonadIO m
       , MonadSystem m
       , MonadProcess m
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
        ExitFailure s -> throwIO $ RestylerExitFailure r s paths

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
