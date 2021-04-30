module Restyler.PullRequest.Status
    ( PullRequestStatus(..)
    , sendPullRequestStatus
    )
where

import Restyler.Prelude

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
    | ErrorStatus URL
    -- ^ We encountered an error and can link to a Job

-- | Send a @'PullRequestStatus'@ for the original Pull Request
sendPullRequestStatus
    :: (HasLogFunc env, HasConfig env, HasPullRequest env, HasGitHub env)
    => PullRequestStatus
    -> RIO env ()
sendPullRequestStatus status =
    whenConfig ((`shouldSendStatus` status) . cStatuses) $ do
        pullRequest <- view pullRequestL
        createHeadShaStatus pullRequest status

createHeadShaStatus
    :: (HasLogFunc env, HasGitHub env)
    => PullRequest
    -> PullRequestStatus
    -> RIO env ()
createHeadShaStatus pullRequest status = do
    logInfo $ "Setting status of " <> shortStatus <> " for " <> shortSha
    runGitHub_ $ createStatusR owner name sha $ statusToStatus status
  where
    owner = pullRequestOwnerName pullRequest
    name = pullRequestRepoName pullRequest
    sha = mkName Proxy $ pullRequestHeadSha pullRequest
    shortSha = fromString $ take 7 $ unpack $ pullRequestHeadSha pullRequest
    shortStatus = case status of
        SkippedStatus _ _ -> "skipped"
        NoDifferencesStatus _ -> "no differences"
        DifferencesStatus _ -> "differences"
        ErrorStatus _ -> "error"

shouldSendStatus :: Statuses -> PullRequestStatus -> Bool
shouldSendStatus Statuses {..} (SkippedStatus _ _) = sSkipped
shouldSendStatus Statuses {..} (NoDifferencesStatus _) = sNoDifferences
shouldSendStatus Statuses {..} (DifferencesStatus _) = sDifferences
shouldSendStatus Statuses {..} (ErrorStatus _) = sError

statusToStatus :: PullRequestStatus -> NewStatus
statusToStatus (SkippedStatus reason mUrl) = NewStatus
    { newStatusState = StatusSuccess
    , newStatusTargetUrl = mUrl
    , newStatusDescription = Just $ "Skipped (" <> reason <> ")"
    , newStatusContext = Just "restyled"
    }
statusToStatus (NoDifferencesStatus mUrl) = NewStatus
    { newStatusState = StatusSuccess
    , newStatusTargetUrl = mUrl
    , newStatusDescription = Just "No differences"
    , newStatusContext = Just "restyled"
    }
statusToStatus (DifferencesStatus mUrl) = NewStatus
    { newStatusState = StatusFailure
    , newStatusTargetUrl = mUrl
    , newStatusDescription = Just "Restyling found differences"
    , newStatusContext = Just "restyled"
    }
statusToStatus (ErrorStatus url) = NewStatus
    { newStatusState = StatusError
    , newStatusTargetUrl = Just url
    , newStatusDescription = Just "Error restyling"
    , newStatusContext = Just "restyled"
    }
