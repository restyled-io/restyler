-- | @restyle PATH [PATH...]@
module Restyler.Local
  ( NullPullRequest (..)
  , run
  ) where

import Restyler.Prelude

import Restyler.App.Class
  ( MonadDownloadFile (..)
  , MonadSystem (..)
  )
import Restyler.Config
import Restyler.Docker (MonadDocker)
import Restyler.Git (MonadGit)
import Restyler.GitHub.PullRequest
import Restyler.Ignore
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.Manifest
import Restyler.Options.NoCommit
import Restyler.Restrictions
import Restyler.RestyleResult
import Restyler.Restyler.Run
import Restyler.RestylerResult

run
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadDownloadFile m
     , MonadSystem m
     , MonadGit m
     , MonadDocker m
     , MonadReader env m
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasManifestOption env
     , HasNoCommitOption env
     , HasRestrictions env
     , HasPullRequestState pr
     , HasAuthor pr
     , HasBaseRef pr
     , HasLabelNames pr
     , HasCallStack
     )
  => pr
  -> [FilePath]
  -> m RestyleResult
run pr paths = do
  config <- loadConfig

  let mIgnoredReason =
        getIgnoredReason
          config
          (getAuthor pr)
          (getBaseRef pr)
          (getLabelNames pr)

  case (cEnabled config, getPullRequestState pr) of
    (False, _) ->
      pure $ RestyleSkipped RestyleNotEnabled
    (True, PullRequestClosed) ->
      pure $ RestyleSkipped RestylePullRequestClosed
    (True, PullRequestOpen)
      | Just reason <- mIgnoredReason ->
          pure $ RestyleSkipped $ RestyleIgnored reason
    (True, PullRequestOpen) -> do
      results <- runRestylers config paths
      pure
        $ if any restylerCommittedChanges results
          then RestyleSuccessDifference results
          else RestyleSuccessNoDifference
