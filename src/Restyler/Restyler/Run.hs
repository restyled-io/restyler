module Restyler.Restyler.Run
    ( runRestylers
    , runRestylers_

    -- * Exported for testing only
    , runRestyler
    , runRestyler_
    , filterRestylePaths
    )
where

import Restyler.Prelude

import Data.List (nub)
import Restyler.App.Class
import Restyler.App.Error
import Restyler.Config
import Restyler.Config.Glob (match)
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Delimited
import Restyler.Git
import Restyler.Options
import Restyler.Restyler
import Restyler.RestylerResult

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
    => (Restyler -> [FilePath] -> RIO env b)
    -> Config
    -> [FilePath]
    -> RIO env [b]
runRestylersWith run Config {..} allPaths = do
    paths <- filterM doesFileExist $ filter included allPaths

    logDebug $ "Restylers: " <> displayShow (map rName restylers)
    logDebug $ "Paths: " <> displayShow paths

    for restylers $ \r -> run r =<< filterRestylePaths r paths
  where
    included path = none (`match` path) cExclude
    restylers = filter rEnabled cRestylers

filterRestylePaths
    :: HasSystem env => Restyler -> [FilePath] -> RIO env [FilePath]
filterRestylePaths r = filterM (r `shouldRestyle`)
  where
    Restyler {..} `shouldRestyle` path
        | includePath rInclude path = pure True
        | null rInterpreters = pure False
        | otherwise = do
            contents <- readFile path `catchAny` \_ -> pure ""
            pure $ any (contents `hasInterpreter`) rInterpreters

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
runRestyler_ r paths =
    case rDelimiters r of
        Nothing -> runRestyler' r paths
        Just ds -> restyleDelimited ds (runRestyler' r) paths

runRestyler'
    :: (HasLogFunc env, HasOptions env, HasSystem env, HasProcess env)
    => Restyler
    -> [FilePath]
    -> RIO env ()
runRestyler' r@Restyler {..} paths =
    if rSupportsMultiplePaths
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
