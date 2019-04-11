module Restyler.PullRequest.Status
    ( PullRequestStatus(..)
    , sendPullRequestStatus
    , sendPullRequestStatus_
    )
where

import Restyler.Prelude

import Restyler.App
import Restyler.Config
import Restyler.Config.Statuses
import Restyler.PullRequest
import GitHub.Endpoints.Repos.Statuses

data PullRequestStatus
    = NoDifferencesStatus
    -- ^ We found no differences after restyling
    | DifferencesStatus URL
    -- ^ We found differences and opened a restyled @'PullRequest'@
    | ErrorStatus URL
    -- ^ We encountered an error and can link to a Job

-- | Send a @'PullRequestStatus'@ for the original Pull Request
sendPullRequestStatus
    :: (HasConfig env, HasPullRequest env, HasGitHub env)
    => PullRequestStatus
    -> RIO env ()
sendPullRequestStatus status =
    whenConfig ((`shouldSendStatus` status) . cStatuses) $ do
        pullRequest <- view pullRequestL
        createHeadShaStatus pullRequest status

createHeadShaStatus
    :: HasGitHub env => PullRequest -> PullRequestStatus -> RIO env ()
createHeadShaStatus pullRequest =
    runGitHub_ . createStatusR owner name sha . statusToStatus
  where
    owner = pullRequestOwnerName pullRequest
    name = pullRequestRepoName pullRequest
    sha = mkName Proxy $ pullRequestHeadSha pullRequest

-- | @'sendPullRequestStatus'@ but ignore any exceptions
--
-- This is useful for emitting the Errored status, where we wouldn't want an
-- exception here to muddy the debugging of the error we're reporting.
--
sendPullRequestStatus_
    :: (HasLogFunc env, HasConfig env, HasPullRequest env, HasGitHub env)
    => PullRequestStatus
    -> RIO env ()
sendPullRequestStatus_ status = sendPullRequestStatus status
    `catchAny` \_ -> logWarn "Error sending PR status"

shouldSendStatus :: Statuses -> PullRequestStatus -> Bool
shouldSendStatus Statuses {..} NoDifferencesStatus = sNoDifferences
shouldSendStatus Statuses {..} (DifferencesStatus _) = sDifferences
shouldSendStatus Statuses {..} (ErrorStatus _) = sError

statusToStatus :: PullRequestStatus -> NewStatus
statusToStatus NoDifferencesStatus = NewStatus
    { newStatusState = StatusSuccess
    , newStatusTargetUrl = Nothing
    , newStatusDescription = Just "No differences"
    , newStatusContext = Just "restyled"
    }
statusToStatus (DifferencesStatus url) = NewStatus
    { newStatusState = StatusFailure
    , newStatusTargetUrl = Just url
    , newStatusDescription = Just "Restyling found differences"
    , newStatusContext = Just "restyled"
    }
statusToStatus (ErrorStatus url) = NewStatus
    { newStatusState = StatusError
    , newStatusTargetUrl = Just url
    , newStatusDescription = Just "Error restyling"
    , newStatusContext = Just "restyled"
    }
