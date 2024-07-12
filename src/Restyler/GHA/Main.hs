{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GHA.Main
  ( main
  ) where

import Restyler.Prelude

import Blammo.Logging.Simple
import Restyler.App (runAppT)
import Restyler.Config
import Restyler.ManifestOption

newtype App = App
  { logger :: Logger
  }

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasManifestOption App where
  manifestOptionL = lens (const $ toManifestOption Nothing) const

main :: IO ()
main = do
  app <- App <$> newLoggerEnv

  runAppT app $ do
    config <- loadConfig
    logInfo $ "Loaded config" :# ["config" .= config]

-- case options . pullRequest of
--   Nothing -> restyler config options . paths
--   Just pr
--     | pr . state == Closed -> do
--         -- TODO close sibling
--         logInfo "PR is closed"
--     | Just reason <- ignorePullRequest config pr -> do
--         logInfo $ "Ignoring PR" :# ["reason" .= reason]
--     | otherwise -> do
--         changed <- filter (`elem` [Added, Modified] . (. status)) $ pr . files
--         restyler config $ changed <> options . paths

-- restyler :: Config -> [FilePath] -> m ()
-- restyler config paths = do
-- plan <- buildRestylePlan config paths
-- logInfo $ "Built restyle plan" :# ["plan" .= plan]
