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

import Data.List (nub)
import Restyler.App.Class
import Restyler.App.Error
import Restyler.Config
import Restyler.Config.ChangedPaths
import Restyler.Config.Glob (match)
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Git
import Restyler.Options
import Restyler.Restyler
import Restyler.RestylerResult
import RIO.FilePath ((</>))

-- | Runs the configured @'Restyler'@s for the files and reports results
runRestylers
    :: ( HasLogFunc env
       , HasOptions env
       , HasSystem env
       , HasProcess env
       , HasGit env
       )
    => Config
    -> [FilePath]
    -> RIO env [RestylerResult]
runRestylers = runRestylersWith runRestyler

-- | @'runRestylers'@, but without committing or reporting results
runRestylers_
    :: (HasLogFunc env, HasOptions env, HasSystem env, HasProcess env)
    => Config
    -> [FilePath]
    -> RIO env ()
runRestylers_ config = void . runRestylersWith runRestyler_ config

runRestylersWith
    :: (HasSystem env, HasLogFunc env)
    => (Restyler -> [FilePath] -> RIO env a)
    -> Config
    -> [FilePath]
    -> RIO env [a]
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
                throwIO $ RestyleError $ utf8BuilderToText maxPathsLogMessage
        else withFilteredPaths restylers paths run
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
    :: HasSystem env
    => [Restyler]
    -> [FilePath]
    -> (Restyler -> [FilePath] -> RIO env a)
    -> RIO env [a]
withFilteredPaths restylers paths run = do
    withInterpreters <- traverse addExecutableInterpreter paths
    for restylers $ \r ->
        run r $ map fst $ filter (r `shouldRestyle`) withInterpreters

addExecutableInterpreter
    :: HasSystem env => FilePath -> RIO env (FilePath, Maybe Interpreter)
addExecutableInterpreter path = handleAny (const $ pure (path, Nothing)) $ do
    isExec <- isFileExecutable path

    (path, ) <$> if isExec
        then readInterpreter <$> readFile path
        else pure Nothing

shouldRestyle :: Restyler -> (FilePath, Maybe Interpreter) -> Bool
Restyler {..} `shouldRestyle` (path, mInterpreter)
    | matchesInterpreter = includePath (explicit path : rInclude) path
    | otherwise = includePath rInclude path
  where
    matchesInterpreter = fromMaybe False $ do
        interpreter <- mInterpreter
        pure $ interpreter `elem` rInterpreters

-- | Run a @'Restyler'@ and get the result (i.e. commit changes)
runRestyler
    :: ( HasLogFunc env
       , HasOptions env
       , HasSystem env
       , HasProcess env
       , HasGit env
       )
    => Restyler
    -> [FilePath]
    -> RIO env RestylerResult
runRestyler r [] = pure $ noPathsRestylerResult r
runRestyler r paths = do
    runRestyler_ r paths
    getRestylerResult r

-- | Run a @'Restyler'@ (don't commit anything)
runRestyler_
    :: (HasLogFunc env, HasOptions env, HasSystem env, HasProcess env)
    => Restyler
    -> [FilePath]
    -> RIO env ()
runRestyler_ _ [] = pure ()
runRestyler_ r@Restyler {..} paths = if rSupportsMultiplePaths
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
    :: (HasOptions env, HasSystem env, HasProcess env)
    => Restyler
    -> [FilePath]
    -> RIO env ()
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

getHostDirectory :: (HasOptions env, HasSystem env) => RIO env FilePath
getHostDirectory = do
    mHostDirectory <- oHostDirectory <$> view optionsL
    maybe getCurrentDirectory pure mHostDirectory

-- | Expand directory arguments and filter to only existing paths
--
-- The existence filtering is important for normal Restyling, where we may get
-- path arguments of removed files in the PR. The expansion is important for
-- @restyle-path@, where we may be given directories as arguments.
--
findFiles :: HasSystem env => [FilePath] -> RIO env [FilePath]
findFiles = fmap concat . traverse go
  where
    go parent = do
        isDirectory <- doesDirectoryExist parent

        if isDirectory
            then do
                files <- listDirectory parent
                findFiles $ map (parent </>) files
            else do
                isFile <- doesFileExist parent
                pure [ parent | isFile ] -- too clever?
