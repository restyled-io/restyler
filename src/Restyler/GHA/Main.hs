{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GHA.Main
  ( main
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.App (AppT, runAppT)
import Restyler.App.Class (MonadProcess (..))
import Restyler.Config
import Restyler.GHA.Event
import Restyler.GHA.Options
import Restyler.Git (MonadGit (..))
import Restyler.HostDirectoryOption
import Restyler.ImageCleanupOption
import Restyler.ManifestOption
import Restyler.PullRequest.File
import Restyler.Restrictions
import Restyler.Restyler.Run (runRestylers)
import UnliftIO.Exception (handleAny)

data App = App
  { logger :: Logger
  , options :: Options
  }

optionsL :: Lens' App Options
optionsL = lens (.options) $ \x y -> x {options = y}

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasManifestOption App where
  manifestOptionL = lens (const $ toManifestOption Nothing) const

instance HasHostDirectoryOption App where
  hostDirectoryOptionL = optionsL . hostDirectoryOptionL

instance HasImageCleanupOption App where
  imageCleanupOptionL = lens (const $ toImageCleanupOption False) const

instance HasRestrictions App where
  restrictionsL = optionsL . restrictionsL

instance MonadUnliftIO m => MonadGit (AppT App m) where
  gitPush branch = callProcess "git" ["push", "origin", branch]
  gitPushForce branch =
    callProcess "git" ["push", "--force", "origin", branch]
  gitDiffNameOnly mRef = do
    let args = ["diff", "--name-only"] <> maybeToList mRef
    map unpack . lines . pack <$> readProcess "git" args
  gitFormatPatch mRef = do
    let args = ["format-patch", "--stdout"] <> maybeToList mRef
    pack <$> readProcess "git" args
  gitCommitAll msg = do
    callProcess "git" ["commit", "-a", "--message", msg]
    unpack
      . T.dropWhileEnd isSpace
      . pack
      <$> readProcess
        "git"
        ["rev-parse", "HEAD"]
  gitCheckout branch = do
    callProcess "git" ["checkout", "--no-progress", "-b", branch]

main :: IO ()
main = do
  options <- parseOptions

  withLogger options.logSettings $ \logger -> do
    let app = App {logger = logger, options = options}

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

      results <- runRestylers config $ mapMaybe pullRequestFileToChangedPath prFiles
      traverse_ (logInfo . ("Result" :#) . objectToPairs) results

logExit :: (MonadIO m, MonadLogger m) => SomeException -> m a
logExit ex = do
  logError $ pack (displayException ex) :# []
  exitFailure
