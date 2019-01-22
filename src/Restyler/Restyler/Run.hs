{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Restyler.Run
    ( runRestylers
    ) where

import Restyler.Prelude

import Data.List (nub)
import Restyler.App
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.RestylerResult

-- | Runs the given @'Restyler'@s over the files
runRestylers :: MonadApp m => [Restyler] -> [FilePath] -> m [RestylerResult]
runRestylers restylers allPaths = do
    paths <- filterM doesFileExist allPaths

    logDebugN $ "Restylers: " <> tshow (map rName restylers)
    logDebugN $ "Paths: " <> tshow paths

    for restylers $ \r -> runRestyler r =<< filterRestylePaths r paths

runRestyler :: MonadApp m => Restyler -> [FilePath] -> m RestylerResult
runRestyler r [] = pure $ noPathsRestylerResult r
runRestyler r@Restyler {..} paths = do
    if rSupportsMultiplePaths
        then do
            logInfoN $ "Restyling " <> tshow paths <> " via " <> pack rName
            dockerRunRestyler r paths
        else for_ paths $ \path -> do
            logInfoN $ "Restyling " <> tshow path <> " via " <> pack rName
            dockerRunRestyler r [path]

    getRestylerResult r

filterRestylePaths :: MonadApp m => Restyler -> [FilePath] -> m [FilePath]
filterRestylePaths r = filterM (r `shouldRestyle`)
  where
    Restyler {..} `shouldRestyle` path
        | includePath rInclude path = pure True
        | null rInterpreters = pure False
        | otherwise = do
            contents <- readFile path
            pure $ any (contents `hasInterpreter`) rInterpreters

dockerRunRestyler :: MonadApp m => Restyler -> [FilePath] -> m ()
dockerRunRestyler Restyler {..} paths = do
    cwd <- getCurrentDirectory
    callProcess "docker"
        $ ["run", "--rm", "--net", "none", "--volume", cwd <> ":/code", rImage]
        <> nub (rCommand <> rArguments)
        <> [ "--" | rSupportsArgSep ]
        <> map ("./" <>) paths
