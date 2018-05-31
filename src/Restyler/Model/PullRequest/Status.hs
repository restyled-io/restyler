{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Model.PullRequest.Status
    ( PullRequestStatus(..)
    , sendPullRequestStatus
    , sendPullRequestStatus_
    )
where

import Restyler.Prelude

import Restyler.App
import Restyler.Capabilities.GitHub
import Restyler.Model.Config
import Restyler.Model.PullRequest
import Restyler.Model.StatusesConfig

data PullRequestStatus
    = NoDifferencesStatus
    -- ^ We found no differences after restyling
    | DifferencesStatus PullRequest
    -- ^ We found differences and opened a restyled @'PullRequest'@
    | ErrorStatus URL
    -- ^ We encountered an error and can link to a Job

-- | Send a @'PullRequestStatus'@ for the original Pull Request
sendPullRequestStatus
    :: (MonadGitHub m, MonadReader App m) => PullRequestStatus -> m ()
sendPullRequestStatus status = do
    statusConfig <- asks $ cStatusesConfig . appConfig

    when (shouldSendStatus statusConfig status) $ do
        pullRequest <- asks appPullRequest
        createStatus
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)
            (mkName Proxy $ pullRequestCommitSha $ pullRequestHead pullRequest)
            (statusToStatus status)

-- | @'sendPullRequestStatus'@ but ignore any exceptions
--
-- This is useful for emitting the Errored status, where we wouldn't want an
-- exception here to muddy the debugging of the error we're reporting.
--
sendPullRequestStatus_
    :: (MonadGitHub m, MonadError AppError m, MonadLogger m, MonadReader App m)
    => PullRequestStatus
    -> m ()
sendPullRequestStatus_ status = sendPullRequestStatus status
    `catchError` \_ -> logWarnN "Error sending PR status"

shouldSendStatus :: StatusesConfig -> PullRequestStatus -> Bool
shouldSendStatus StatusesConfig {..} NoDifferencesStatus = scNoDifferences
shouldSendStatus StatusesConfig {..} (DifferencesStatus _) = scDifferences
shouldSendStatus StatusesConfig {..} (ErrorStatus _) = scError

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
