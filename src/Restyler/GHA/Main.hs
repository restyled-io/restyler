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
import UnliftIO.Exception (handleAny)

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

    runAppT app $ handleAny logExit $ do
      config <- loadConfig
      logInfo $ "Loaded config" :# objectToPairs config

      githubEvent <- decodeJsonThrow @_ @Event options.githubEventJson
      logInfo $ "Handling PR" :# objectToPairs githubEvent.payload

      -- TODO
      -- check draft
      -- check closed
      -- check ignores

      prFiles <- decodeJsonThrow @_ @[PullRequestFile] options.githubPRFilesJson
      traverse_ (logInfo . ("Changed file" :#) . objectToPairs) prFiles

logExit :: (MonadIO m, MonadLogger m) => SomeException -> m a
logExit ex = do
  logError $ pack (displayException ex) :# []
  exitFailure
