module Restyler.Restyler.Run
    ( runRestylers
    ) where

import Restyler.Prelude

import Data.List (nub)
import Restyler.App.Class
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.RestylerResult

-- | Runs the given @'Restyler'@s over the files
runRestylers
    :: (HasLogFunc env, HasSystem env, HasProcess env)
    => [Restyler]
    -> [FilePath]
    -> RIO env [RestylerResult]
runRestylers restylers allPaths = do
    paths <- filterM doesFileExist allPaths

    logDebug $ "Restylers: " <> displayShow (map rName restylers)
    logDebug $ "Paths: " <> displayShow paths

    for restylers $ \r -> runRestyler r =<< filterRestylePaths r paths

runRestyler
    :: (HasLogFunc env, HasSystem env, HasProcess env)
    => Restyler
    -> [FilePath]
    -> RIO env RestylerResult
runRestyler r [] = pure $ noPathsRestylerResult r
runRestyler r@Restyler {..} paths = do
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

    getRestylerResult r

filterRestylePaths
    :: HasSystem env => Restyler -> [FilePath] -> RIO env [FilePath]
filterRestylePaths r = filterM (r `shouldRestyle`)
  where
    Restyler {..} `shouldRestyle` path
        | includePath rInclude path = pure True
        | null rInterpreters = pure False
        | otherwise = do
            contents <- readFile path
            pure $ any (contents `hasInterpreter`) rInterpreters

dockerRunRestyler
    :: (HasSystem env, HasProcess env) => Restyler -> [FilePath] -> RIO env ()
dockerRunRestyler Restyler {..} paths = do
    cwd <- getCurrentDirectory
    callProcess "docker"
        $ ["run", "--rm", "--net", "none", "--volume", cwd <> ":/code", rImage]
        <> nub (rCommand <> rArguments)
        <> [ "--" | rSupportsArgSep ]
        <> map ("./" <>) paths
