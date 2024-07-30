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
import Restyler.Options.NoPull
import Restyler.Restrictions
import Restyler.RestyleResult
import Restyler.Restyler
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
     , HasNoPullOption env
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
    (False, _) -> do
      pure $ RestyleSkipped RestyleNotEnabled
    (True, PullRequestClosed) ->
      pure $ RestyleSkipped RestylePullRequestClosed
    (True, PullRequestOpen)
      | Just reason <- mIgnoredReason ->
          pure $ RestyleSkipped $ RestyleIgnored reason
    (True, PullRequestOpen) -> do
      mresults <- runRestylers config paths

      -- Overall result is logged in CLI, log individual results here
      for_ mresults $ traverse_ $ \result -> do
        logDebug
          $ "RestylerResult"
          :# [ "restyler" .= rName (result.restyler)
             , "restyled" .= result.restyled
             , "sha" .= result.sha
             ]

      pure $ maybe RestyleNoDifference (const RestyleDifference) mresults
