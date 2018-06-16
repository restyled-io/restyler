{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Model.Restyler.Run
    ( runRestylers
    ) where

import Restyler.Prelude

import Data.List (nub)
import Restyler.Capabilities.Docker
import Restyler.Capabilities.System
import Restyler.Model.Include
import Restyler.Model.Interpreter
import Restyler.Model.Restyler

-- | Runs the given @'Restyler'@s over the files
--
-- Returns the subset of @'Restyler@'s that were actually invoked.
--
runRestylers
    :: (Monad m, MonadSystem m, MonadDocker m, MonadLogger m)
    => [Restyler]
    -> [FilePath]
    -> m [Restyler]
runRestylers restylers allPaths = do
    paths <- filterM doesFileExist allPaths

    logDebugN $ "Restylers: " <> tshow (map rName restylers)
    logDebugN $ "Paths: " <> tshow paths

    filterM (\r -> runRestyler r =<< filterRestylePaths r paths) restylers

runRestyler
    :: (MonadSystem m, Monad m, MonadDocker m, MonadLogger m)
    => Restyler
    -> [FilePath]
    -> m Bool
runRestyler _ [] = pure False
runRestyler r@Restyler {..} paths = True <$ if rSupportsMultiplePaths
    then do
        logInfoN $ "Restyling " <> tshow paths <> " via " <> pack rName
        dockerRunRestyler r paths
    else for_ paths $ \path -> do
        logInfoN $ "Restyling " <> tshow path <> " via " <> pack rName
        dockerRunRestyler r [path]

filterRestylePaths
    :: (Monad m, MonadSystem m) => Restyler -> [FilePath] -> m [FilePath]
filterRestylePaths r = filterM (r `shouldRestyle`)
  where
    Restyler {..} `shouldRestyle` path
        | includePath rInclude path = pure True
        | null rInterpreters = pure False
        | otherwise = do
            contents <- readFile path
            pure $ any (contents `hasInterpreter`) rInterpreters

dockerRunRestyler
    :: (MonadDocker m, MonadSystem m, Monad m) => Restyler -> [FilePath] -> m ()
dockerRunRestyler Restyler {..} paths = do
    cwd <- getCurrentDirectory

    dockerRun
        $ ["--rm", "--net", "none", "--volume", cwd <> ":/code", rImage]
        <> nub (rCommand <> rArguments)
        <> [ "--" | rSupportsArgSep ]
        <> map ("./" <>) paths
