{-# LANGUAGE OverloadedStrings #-}

module Restyler.PullRequest.Status
    ( PullRequestStatus(..)
    , sendPullRequestStatus
    ) where

import Restyler.Prelude

import GitHub.Client
import GitHub.Data
import Restyler.PullRequest

data PullRequestStatus
    = NoDifferencesStatus
    | ErrorStatus

sendPullRequestStatus :: PullRequest -> PullRequestStatus -> GitHubRW Status
sendPullRequestStatus pullRequest =
    createStatus
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)
            (mkName Proxy $ pullRequestCommitSha $ pullRequestHead pullRequest)
        . statusToStatus

statusToStatus :: PullRequestStatus -> NewStatus
statusToStatus NoDifferencesStatus = NewStatus
    { newStatusState = StatusSuccess
    , newStatusTargetUrl = Nothing
    , newStatusDescription = Just "No differences"
    , newStatusContext = Just "restyled"
    }
statusToStatus ErrorStatus = NewStatus
    { newStatusState = StatusError
    , newStatusTargetUrl = Just
        (URL "https://github.com/restyled-io/restyled.io/wiki/Restyler-Errors")
    , newStatusDescription = Just "Restyling failed"
    , newStatusContext = Nothing
    }
