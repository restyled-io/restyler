-- | @restyle PATH [PATH...]@
module Restyler.Local
  ( NullPullRequest (..)
  , run
  ) where

import Restyler.Prelude

import Restyler.App.Class
  ( MonadDownloadFile (..)
  , MonadProcess (..)
  , MonadSystem (..)
  )
import Restyler.Config
import Restyler.Git (MonadGit)
import Restyler.GitHub.PullRequest
import Restyler.Ignore
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.Manifest
import Restyler.Restrictions
import Restyler.RestyleResult
import Restyler.Restyler.Run

-- | A 'PullRequest'-like object designed to never match the state or ignore
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
  -> m (RestyleResult pr)
run pr paths = do
  config <- loadConfig
  logDebug $ "Config" :# objectToPairs config

  let mIgnoredReason =
        getIgnoredReason
          config
          (getAuthor pr)
          (getBaseRef pr)
          $ getLabelNames pr

  case (cEnabled config, getPullRequestState pr) of
    (False, _) ->
      pure $ RestyleSkipped config pr RestyleNotEnabled
    (True, PullRequestClosed) ->
      pure $ RestyleSkipped config pr RestylePullRequestClosed
    (True, PullRequestOpen)
      | Just reason <- mIgnoredReason ->
          pure $ RestyleSkipped config pr $ RestyleIgnored reason
    (True, PullRequestOpen) -> runRestyle config pr $ runRestylers config paths
