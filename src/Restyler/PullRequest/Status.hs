module Restyler.PullRequest.Status
    ( PullRequestStatus(..)
    , sendPullRequestStatus
    , sendPullRequestStatus'
    , createHeadShaStatus
    ) where

import Restyler.Prelude

import qualified Data.Text as T
import GitHub.Endpoints.Repos.Statuses
import Restyler.App.Class
import Restyler.Config
import Restyler.Config.Statuses
import Restyler.PullRequest

data PullRequestStatus
    = SkippedStatus Text (Maybe URL)
    -- ^ We skipped this PR for some reason
    | NoDifferencesStatus (Maybe URL)
    -- ^ We found no differences after restyling
    | DifferencesStatus (Maybe URL)
    -- ^ We found differences and opened a restyled @'PullRequest'@

-- | Send a @'PullRequestStatus'@ for the original Pull Request
sendPullRequestStatus
    :: ( MonadLogger m
       , MonadGitHub m
       , MonadReader env m
       , HasConfig env
       , HasPullRequest env
       )
    => PullRequestStatus
    -> m ()
sendPullRequestStatus status = do
    config <- view configL
    pullRequest <- view pullRequestL
    sendPullRequestStatus' config pullRequest status

-- | Internals of @'sendPullRequestStatus'@ extracted for non-Reader usage
sendPullRequestStatus'
    :: (MonadLogger m, MonadGitHub m)
    => Config
    -> PullRequest
    -> PullRequestStatus
    -> m ()
sendPullRequestStatus' Config {..} pullRequest status =
    when (cStatuses `shouldSendStatus` status)
        $ createHeadShaStatus pullRequest status

createHeadShaStatus
    :: (MonadLogger m, MonadGitHub m)
    => PullRequest
    -> PullRequestStatus
    -> m ()
createHeadShaStatus pullRequest status = do
    logInfo
        $ "Setting PR status"
        :# ["status" .= shortStatus, "commit" .= shortSha]
    runGitHub_ $ createStatusR owner name sha $ statusToStatus status
  where
    owner = pullRequestOwnerName pullRequest
    name = pullRequestRepoName pullRequest
    sha = mkName Proxy $ pullRequestHeadSha pullRequest

    shortSha :: Text
    shortSha = T.take 7 $ pullRequestHeadSha pullRequest

    shortStatus :: Text
    shortStatus = case status of
        SkippedStatus{} -> "skipped"
        NoDifferencesStatus{} -> "no differences"
        DifferencesStatus{} -> "differences"

shouldSendStatus :: Statuses -> PullRequestStatus -> Bool
shouldSendStatus Statuses {..} = \case
    SkippedStatus{} -> sSkipped
    NoDifferencesStatus{} -> sNoDifferences
    DifferencesStatus{} -> sDifferences

statusToStatus :: PullRequestStatus -> NewStatus
statusToStatus = \case
    SkippedStatus reason mUrl -> NewStatus
        { newStatusState = StatusSuccess
        , newStatusTargetUrl = mUrl
        , newStatusDescription = Just $ "Skipped (" <> reason <> ")"
        , newStatusContext = Just "restyled"
        }
    NoDifferencesStatus mUrl -> NewStatus
        { newStatusState = StatusSuccess
        , newStatusTargetUrl = mUrl
        , newStatusDescription = Just "No differences"
        , newStatusContext = Just "restyled"
        }
    DifferencesStatus mUrl -> NewStatus
        { newStatusState = StatusFailure
        , newStatusTargetUrl = mUrl
        , newStatusDescription = Just "Restyling found differences"
        , newStatusContext = Just "restyled"
        }
