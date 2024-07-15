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

import Restyler.App (AppT, runAppT)
import Restyler.App.Class (MonadDownloadFile, MonadProcess, MonadSystem)
import Restyler.Commands.RestyleLocal qualified as RestyleLocal
import Restyler.Config (loadConfig)
import Restyler.GHA
import Restyler.Git (ActualGit (..), MonadGit)
import Restyler.GitHub.Api
import Restyler.GitHub.PullRequest.File
import Restyler.GitHub.Repository
import Restyler.HostDirectoryOption
import Restyler.ImageCleanupOption
import Restyler.ManifestOption
import Restyler.Options.RestyleGHA
import Restyler.Restrictions
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

instance HasGitHubOutput App where
  githubOutputL = optionsL . githubOutputL

instance HasRestrictions App where
  restrictionsL = optionsL . restrictionsL

instance HasHostDirectoryOption App where
  hostDirectoryOptionL = optionsL . hostDirectoryOptionL

instance HasManifestOption App where
  manifestOptionL = noManifestOptionL

instance HasImageCleanupOption App where
  imageCleanupOptionL = noImageCleanupOptionL

deriving via
  (ActualGit (AppT App m))
  instance
    MonadUnliftIO m => MonadGit (AppT App m)

main :: Repository -> Int -> IO ()
main repo pr = do
  options <- getOptions

  withLogger options.logSettings $ \logger -> do
    let app =
          App
            { logger = logger
            , options = options
            }

    void $ runAppT app $ run repo pr

run
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , -- Needed for our own logic
       HasGitHubToken env
     , HasGitHubOutput env
     , -- Needed for RestyleLocal
       MonadSystem m
     , MonadProcess m
     , MonadGit m
     , MonadDownloadFile m
     , HasRestrictions env
     , HasHostDirectoryOption env
     , HasManifestOption env
     , HasImageCleanupOption env
     , HasManifestOption env
     )
  => Repository
  -> Int
  -> m RestyleResult
run repo pr = do
  pullRequest <- getPullRequest repo pr
  logInfo $ "Handling PR" :# objectToPairs pullRequest

  paths <- mapMaybe pullRequestFileToChangedPath <$> getPullRequestFiles repo pr
  traverse_ (logDebug . ("Path" :#) . objectToPairs) paths

  result <- RestyleLocal.run pullRequest paths
  _differences <- setRestylerResultOutputs pullRequest result
  -- TODO no differences? cleanup PR

  pure result
