module Restyler.PullRequest.Status
    ( PullRequestStatus(..)
    , sendPullRequestStatus
    , errorPullRequest
    )
where

import Restyler.Prelude

import GitHub.Data (NewStatus(..), StatusState(..))
import Restyler.App.Error
import Restyler.Capabilities.GitHub
import Restyler.Config
import Restyler.Config.Statuses
import Restyler.Options
import Restyler.PullRequest

data PullRequestStatus
    = NoDifferencesStatus
    | DifferencesStatus (Maybe URL)
    | ErrorStatus AppError

-- | Send a @'PullRequestStatus'@ for the original Pull Request
sendPullRequestStatus
    :: ( MonadGitHub m
       , MonadReader env m
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       )
    => PullRequestStatus
    -> m ()
sendPullRequestStatus status = do
    whenConfig ((`shouldSendStatus` status) . cStatuses) $ do
        mJobUrl <- oJobUrl <$> view optionsL
        pullRequest <- view pullRequestL
        setPullRequestStatus pullRequest $ statusToStatus mJobUrl status

errorPullRequest
    :: ( MonadGitHub m
       , MonadReader env m
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       )
    => AppError
    -> m ()
errorPullRequest = sendPullRequestStatus . ErrorStatus

shouldSendStatus :: Statuses -> PullRequestStatus -> Bool
shouldSendStatus Statuses {..} NoDifferencesStatus{} = sNoDifferences
shouldSendStatus Statuses {..} DifferencesStatus{} = sDifferences
shouldSendStatus Statuses {..} ErrorStatus{} = sError

statusToStatus :: Maybe URL -> PullRequestStatus -> NewStatus
statusToStatus mUrl NoDifferencesStatus = NewStatus
    { newStatusState = StatusSuccess
    , newStatusTargetUrl = mUrl
    , newStatusDescription = Just "No differences"
    , newStatusContext = Just "restyled"
    }
statusToStatus _ (DifferencesStatus mUrl) = NewStatus
    { newStatusState = StatusFailure
    , newStatusTargetUrl = mUrl
    , newStatusDescription = Just "Restyling found differences"
    , newStatusContext = Just "restyled"
    }
statusToStatus mUrl (ErrorStatus _e) = NewStatus
    { newStatusState = StatusError
    , newStatusTargetUrl = mUrl
    , newStatusDescription = Just "Error restyling"
    , newStatusContext = Just "restyled"
    }
