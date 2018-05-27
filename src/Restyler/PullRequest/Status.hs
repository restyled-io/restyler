{-# LANGUAGE OverloadedStrings #-}

module Restyler.PullRequest.Status
    ( PullRequestStatus(..)
    , sendPullRequestStatus
    , sendPullRequestStatusError
    )
where

import Restyler.Prelude

import GitHub.Data
import GitHub.Endpoints.Repos.Statuses
import Restyler.GitHub
import Restyler.PullRequest

data PullRequestStatus
    = NoDifferencesStatus
    | ErrorStatus URL

sendPullRequestStatus :: PullRequestStatus -> AppM ()
sendPullRequestStatus status = do
    pullRequest <- asks appPullRequest
    runGitHub_ $ createStatusR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (mkName Proxy $ pullRequestCommitSha $ pullRequestHead pullRequest)
        (statusToStatus status)

sendPullRequestStatusError :: URL -> AppM ()
sendPullRequestStatusError url =
    sendPullRequestStatus (ErrorStatus url)
        `catchErrorWarn` "Unable to send PR errored status"
    where
    -- The assumption is we're always calling this in the context of an
    -- error-handler, so if we ourselves throw an error we shouldn't mask
    -- whatever we're handling.
          f `catchErrorWarn` msg = f `catchError` const (logWarnN msg)

statusToStatus :: PullRequestStatus -> NewStatus
statusToStatus NoDifferencesStatus = NewStatus
    { newStatusState = StatusSuccess
    , newStatusTargetUrl = Nothing
    , newStatusDescription = Just "No differences"
    , newStatusContext = Just "restyled"
    }
statusToStatus (ErrorStatus url) = NewStatus
    { newStatusState = StatusError
    , newStatusTargetUrl = Just url
    , newStatusDescription = Just "Error restyling"
    , newStatusContext = Just "restyled"
    }
