{-# LANGUAGE OverloadedStrings #-}

module Restyler.PullRequest.Status
    ( PullRequestStatus(..)
    , sendPullRequestStatus
    , sendPullRequestStatus_
    )
where

import Restyler.Prelude

import GitHub.Data
import GitHub.Endpoints.Repos.Statuses
import Restyler.GitHub
import Restyler.PullRequest

data PullRequestStatus
    = NoDifferencesStatus
    | DifferencesStatus PullRequest
    | ErrorStatus URL

sendPullRequestStatus :: PullRequestStatus -> AppM ()
sendPullRequestStatus status = do
    pullRequest <- asks appPullRequest
    runGitHub_ $ createStatusR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (mkName Proxy $ pullRequestCommitSha $ pullRequestHead pullRequest)
        (statusToStatus status)

-- | @'sendPullRequestStatus'@ but ignore any exceptions
--
-- This is useful for emitting the Errored status, where we wouldn't want an
-- exception here to muddy the debugging of the error we're reporting.
--
sendPullRequestStatus_ :: PullRequestStatus -> AppM ()
sendPullRequestStatus_ status = sendPullRequestStatus status
    `catchError` \_ -> logWarnN "Error sending PR status"

statusToStatus :: PullRequestStatus -> NewStatus
statusToStatus NoDifferencesStatus = NewStatus
    { newStatusState = StatusSuccess
    , newStatusTargetUrl = Nothing
    , newStatusDescription = Just "No differences"
    , newStatusContext = Just "restyled"
    }
statusToStatus (DifferencesStatus pullRequest) = NewStatus
    { newStatusState = StatusFailure
    , newStatusTargetUrl = Just $ URL $ pullRequestURL pullRequest
    , newStatusDescription = Just "Restyling found differences"
    , newStatusContext = Just "restyled"
    }
statusToStatus (ErrorStatus url) = NewStatus
    { newStatusState = StatusError
    , newStatusTargetUrl = Just url
    , newStatusDescription = Just "Error restyling"
    , newStatusContext = Just "restyled"
    }
