{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GHA.Main
  ( main
  ) where

import Restyler.Prelude

import Restyler.App (runAppT)
import Restyler.Config
import Restyler.GHA.Event
import Restyler.GHA.Options
import Restyler.ManifestOption
import Restyler.PullRequest.File
import UnliftIO.Exception (tryAny)

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

  withLogger options.logSettings $ \logger -> do
    let app = App {logger = logger}

    runAppT app $ do
      result <- tryAny $ do
        config <- loadConfig
        logInfo $ "Loaded config" :# objectToPairs config

        githubEvent <- decodeJsonThrow @_ @Event options.githubEventJson
        logInfo $ "Handling PR" :# objectToPairs githubEvent.payload

        prFiles <- decodeJsonThrow @_ @[PullRequestFile] options.githubEventJson
        traverse_ (logInfo . ("Changed file" :#) . objectToPairs) prFiles

      -- check draft
      -- check closed
      -- check ignores
      -- restyle, making commits

      case result of
        Left ex -> do
          logError $ pack (displayException ex) :# []
          exitFailure
        Right () -> pure ()
