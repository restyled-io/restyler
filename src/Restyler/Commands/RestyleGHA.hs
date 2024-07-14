-- |
--
-- - Fetch PR details
-- - Check closed, ignore, etc
-- - Run @RestyleLocal@ with commits
module Restyler.Commands.RestyleGHA
  ( main
  , run
  ) where

import Restyler.Prelude

import Restyler.App (runAppT)
import Restyler.App.Class (MonadDownloadFile, MonadSystem)
import Restyler.Commands.RestyleLocal qualified as RestyleLocal
import Restyler.Config (loadConfig)
import Restyler.GitHub.Api
import Restyler.GitHub.PullRequest.File
import Restyler.GitHub.Repository
import Restyler.ManifestOption
import Restyler.Options.RestyleGHA
import Restyler.RestyleResult

data App = App
  { logger :: Logger
  , options :: Options
  }

optionsL :: Lens' App Options
optionsL = lens (.options) $ \x y -> x {options = y}

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasGitHubToken App where
  githubTokenL = optionsL . githubTokenL

instance HasManifestOption App where
  manifestOptionL = noManifestOptionL

main :: Repository -> Int -> IO ()
main repo pr = do
  options <- getOptions

  withLogger options.logSettings $ \logger -> do
    let app =
          App
            { logger = logger
            , options = options
            }

    runAppT app $ do
      result <- run repo pr

      -- TODO:
      -- write outputs

      logRestyleResult result

run
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadDownloadFile m
     , MonadGitHub m
     , MonadReader env m
     , HasManifestOption env
     )
  => Repository
  -> Int
  -> m RestyleResult
run repo pr = do
  config <- loadConfig
  logDebug $ "Config" :# objectToPairs config

  pullRequest <- getPullRequest repo pr
  logInfo $ "Handling PR" :# objectToPairs pullRequest

  -- TODO
  -- before: check draft
  -- before: check closed
  -- before: check ignores
  -- after: cleanup PR if no diff

  paths <- mapMaybe pullRequestFileToChangedPath <$> getPullRequestFiles repo pr

  RestyleLocal.run paths
