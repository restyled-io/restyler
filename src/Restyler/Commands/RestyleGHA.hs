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
import Restyler.GHA
import Restyler.Git (ActualGit (..), MonadGit)
import Restyler.GitHub.Api
import Restyler.GitHub.PullRequest.File
import Restyler.GitHub.Repository
import Restyler.HostDirectoryOption
import Restyler.ImageCleanupOption
import Restyler.LogSettingsOption
import Restyler.ManifestOption
import Restyler.Options.RestyleGHA
import Restyler.Options.RestyleLocal (Options (..))
import Restyler.Options.RestyleLocal qualified as RestyleLocal
import Restyler.Restrictions
import Restyler.RestyleResult

data App = App
  { logger :: Logger
  , env :: EnvOptions
  , options :: Options
  }

envL :: Lens' App EnvOptions
envL = lens (.env) $ \x y -> x {env = y}

optionsL :: Lens' App Options
optionsL = lens (.options) $ \x y -> x {options = y}

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasGitHubToken App where
  githubTokenL = envL . githubTokenL

instance HasGitHubOutput App where
  githubOutputL = envL . githubOutputL

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

main :: EnvOptions -> RestyleLocal.Options -> Repository -> Int -> IO ()
main env options repo pr = do
  withLogger (resolveLogSettings options.logSettings) $ \logger -> do
    let app =
          App
            { logger = logger
            , env = env
            , options = options
            }

    void $ runAppT app $ run repo pr

run
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadDownloadFile m
     , MonadSystem m
     , MonadProcess m
     , MonadGit m
     , MonadReader env m
     , HasLogger env
     , HasGitHubToken env
     , HasGitHubOutput env
     , HasRestrictions env
     , HasHostDirectoryOption env
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
