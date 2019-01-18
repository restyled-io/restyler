{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

data PullRequestStatus
    = NoDifferencesStatus
    -- ^ We found no differences after restyling
    | DifferencesStatus URL
    -- ^ We found differences and opened a restyled @'PullRequest'@
    | ErrorStatus URL
    -- ^ We encountered an error and can link to a Job

-- | Send a @'PullRequestStatus'@ for the original Pull Request
sendPullRequestStatus :: MonadApp m => PullRequestStatus -> m ()
sendPullRequestStatus status = do
    statusConfig <- asks $ cStatuses . appConfig
    when (shouldSendStatus statusConfig status) $ do
        pullRequest <- asks appPullRequest
        createHeadShaStatus pullRequest status

createHeadShaStatus :: MonadApp m => PullRequest -> PullRequestStatus -> m ()
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
sendPullRequestStatus_ :: MonadApp m => PullRequestStatus -> m ()
sendPullRequestStatus_ status = sendPullRequestStatus status
    `catchError` \_ -> logWarnN "Error sending PR status"

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
