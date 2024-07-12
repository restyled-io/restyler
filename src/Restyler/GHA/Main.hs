{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GHA.Main
  ( main
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Restyler.App (runAppT)
import Restyler.Config
import Restyler.GHA.Event
import Restyler.GHA.Options
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
  options <- parseOptions
  logger <- newLogger options.logSettings

  let app =
        App
          { logger = logger
          }

  runAppT app $ do
    config <- loadConfig
    logInfo $ "Loaded config" :# objectToPairs config

    githubEvent <- decodeJsonThrow @_ @Event options.githubEventJson
    logInfo $ "Handling PR" :# objectToPairs githubEvent.payload

objectToPairs :: (ToJSON a, KeyValue kv) => a -> [kv]
objectToPairs a = case toJSON a of
  Object km -> map (uncurry (.=)) $ KeyMap.toList km
  _ -> []

decodeJsonThrow :: (MonadIO m, FromJSON a) => FilePath -> m a
decodeJsonThrow =
  either throwString pure <=< liftIO . eitherDecodeFileStrict'

-- (either throwString pure <=< traverse eitherDecodeFileStrict')

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
