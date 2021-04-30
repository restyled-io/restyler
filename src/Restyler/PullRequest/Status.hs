module Restyler.PullRequest.Status
    ( PullRequestStatus(..)
    , sendPullRequestStatus
    , sendPullRequestStatus'
    ) where

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
sendPullRequestStatus status = do
    config <- view configL
    pullRequest <- view pullRequestL
    sendPullRequestStatus' config pullRequest status

-- | Internals of @'sendPullRequestStatus'@ extracted for non-Reader usage
sendPullRequestStatus'
    :: (HasLogFunc env, HasGitHub env)
    => Config
    -> PullRequest
    -> PullRequestStatus
    -> RIO env ()
sendPullRequestStatus' Config {..} pullRequest status =
    when (cStatuses `shouldSendStatus` status)
        $ createHeadShaStatus pullRequest status

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
        SkippedStatus{} -> "skipped"
        NoDifferencesStatus{} -> "no differences"
        DifferencesStatus{} -> "differences"
        ErrorStatus{} -> "error"

shouldSendStatus :: Statuses -> PullRequestStatus -> Bool
shouldSendStatus Statuses {..} = \case
    SkippedStatus{} -> sSkipped
    NoDifferencesStatus{} -> sNoDifferences
    DifferencesStatus{} -> sDifferences
    ErrorStatus{} -> sError

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
    ErrorStatus url -> NewStatus
        { newStatusState = StatusError
        , newStatusTargetUrl = Just url
        , newStatusDescription = Just "Error restyling"
        , newStatusContext = Just "restyled"
        }
