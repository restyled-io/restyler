-- | @restyle path...@
--
-- - Run restylers (TODO with or without commits)
module Restyler.Commands.RestyleLocal
  ( main
  , run
  ) where

import Restyler.Prelude

import Restyler.App (AppT, runAppT)
import Restyler.App.Class
  ( MonadDownloadFile (..)
  , MonadProcess (..)
  , MonadSystem (..)
  )
import Restyler.Config
import Restyler.Git (ActualGit (..), MonadGit)
import Restyler.GitHub.PullRequest
import Restyler.HostDirectoryOption
import Restyler.Ignore
import Restyler.ImageCleanupOption
import Restyler.LogSettingsOption
import Restyler.ManifestOption
import Restyler.Options.RestyleLocal
import Restyler.Restrictions
import Restyler.RestyleResult
import Restyler.Restyler.Run

-- | A 'PullRequest'-like object designed to never match the statue or ignore
-- checks we do here when running against a real PR.
data NullPullRequest = NullPullRequest

instance HasPullRequestState NullPullRequest where
  getPullRequestState = const PullRequestOpen

instance HasAuthor NullPullRequest where
  getAuthor = const "NONE"

instance HasBaseRef NullPullRequest where
  getBaseRef = const "UNKNOWN"

instance HasLabelNames NullPullRequest where
  getLabelNames = const []

data App = App
  { logger :: Logger
  , options :: Options
  }

optionsL :: Lens' App Options
optionsL = lens (.options) $ \x y -> x {options = y}

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

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

main :: NonEmpty FilePath -> IO ()
main paths = do
  options <- getOptions

  withLogger (resolveLogSettings options.logSettings) $ \logger -> do
    let app =
          App
            { logger = logger
            , options = options
            }

    void $ runAppT app $ run NullPullRequest $ toList paths

run
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadDownloadFile m
     , MonadSystem m
     , MonadProcess m
     , MonadGit m
     , MonadReader env m
     , HasLogger env
     , HasRestrictions env
     , HasHostDirectoryOption env
     , HasManifestOption env
     , HasImageCleanupOption env
     , HasPullRequestState pr
     , HasAuthor pr
     , HasBaseRef pr
     , HasLabelNames pr
     )
  => pr
  -> [FilePath]
  -> m RestyleResult
run pr paths = do
  config <- loadConfig
  logDebug $ "Config" :# objectToPairs config

  result <- do
    case getPullRequestState pr of
      PullRequestClosed -> pure RestyleSkippedClosed
      PullRequestOpen -> do
        maybe
          (Restyled <$> runRestylers config paths)
          (pure . RestyleSkippedIgnored)
          $ getIgnoredReason' config (getAuthor pr) (getBaseRef pr)
          $ getLabelNames pr

  result <$ logRestyleResult result
