{-# LANGUAGE UndecidableInstances #-}

module Restyler.Capabilities.GitHub
    ( MonadGitHub(..)
    , runGitHub_
    , runGitHubFirst
    , ActualGitHub(..)
    ) where

import Restyler.Prelude

import qualified Data.Vector as V
import GitHub.Auth
import GitHub.Data
    ( IssueComment(..)
    , IssueLabel(..)
    , IssueNumber(..)
    , IssueState(..)
    , NewStatus
    , Owner
    , Repo
    )
import GitHub.Data.Request
import GitHub.Endpoints.GitData.References (deleteReferenceR)
import GitHub.Endpoints.Issues.Comments
    (commentsR, createCommentR, deleteCommentR)
import GitHub.Endpoints.Issues.Labels (labelsOnIssueR)
import GitHub.Endpoints.PullRequests
    (EditPullRequest(..), pullRequestR, updatePullRequestR)
import GitHub.Endpoints.Repos.Statuses (createStatusR)
import GitHub.Request
import GitHub.Request.Display
import Network.HTTP.Client.TLS
import Restyler.App.Error
import Restyler.Capabilities.Logger
import Restyler.Options
import Restyler.PullRequest

class Monad m => MonadGitHub m where
    runGitHub :: forall t k a. ParseResponse t a => GenRequest t k a -> m a

    getPullRequest
        :: Name Owner
        -> Name Repo
        -> IssueNumber
        -> m PullRequest
    getPullRequest owner repo number = runGitHub $ pullRequestR
            owner
            repo
            number

    getPullRequestComments
        :: PullRequest
        -> m (Vector IssueComment)
    getPullRequestComments pullRequest = runGitHub $ commentsR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestNumber pullRequest)
        FetchAll

    createPullRequestComment
        :: PullRequest
        -> Text
        -> m ()
    createPullRequestComment pullRequest body =
        void $ runGitHub $ createCommentR
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)
            (pullRequestNumber pullRequest)
            body

    deletePullRequestComment
        :: PullRequest
        -> IssueComment
        -> m ()
    deletePullRequestComment pullRequest comment =
        void $ runGitHub $ deleteCommentR
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)
            (mkId Proxy $ issueCommentId comment)

    getPullRequestLabelNames
        :: PullRequest -> m (Vector (Name IssueLabel))
    getPullRequestLabelNames pullRequest = do
        labels <- runGitHub $ labelsOnIssueR
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)
            (pullRequestIssueId pullRequest)
            FetchAll
        pure $ labelName <$> labels

    setPullRequestStatus
        :: PullRequest
        -> NewStatus
        -> m ()
    setPullRequestStatus pullRequest status = void $ runGitHub $ createStatusR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (mkName Proxy $ pullRequestHeadSha pullRequest)
        status

    deleteHeadRef
        :: IsPullRequest pr
        => pr
        -> m ()
    deleteHeadRef pr = void $ runGitHub $ deleteReferenceR
        (pullRequestOwnerName pr)
        (pullRequestRepoName pr)
        (mkName Proxy $ "heads/" <> pullRequestHeadRef pr)

    editPullRequestState
        :: IsPullRequest pr
        => IssueState
        -> pr
        -> m ()
    editPullRequestState state pr =
        when (pullRequestState pr /= state)
            $ void
            $ runGitHub
            $ updatePullRequestR
                (pullRequestOwnerName pr)
                (pullRequestRepoName pr)
                (pullRequestNumber pr)
                EditPullRequest
                    { editPullRequestTitle = Nothing
                    , editPullRequestBody = Nothing
                    , editPullRequestState = Just state
                    , editPullRequestBase = Nothing
                    , editPullRequestMaintainerCanModify = Nothing
                    }

instance MonadGitHub m => MonadGitHub (ExceptT e m) where
    runGitHub = lift . runGitHub

-- | Fetch the first page using 'runGitHub', return the first item
runGitHubFirst
    :: (MonadGitHub m, ParseResponse t (Vector a))
    => (FetchCount -> GenRequest t k (Vector a))
    -> m (Maybe a)
runGitHubFirst f = (V.!? 0) <$> runGitHub (f 1)

-- | 'void' . 'runGitHub'
runGitHub_ :: (MonadGitHub m, ParseResponse t a) => GenRequest t k a -> m ()
runGitHub_ = void . runGitHub

newtype ActualGitHub m a = ActualGitHub
    { unActualGitHub :: m a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadLogger
        , MonadReader env
        )

instance (MonadIO m, MonadLogger m, MonadReader env m, HasOptions env)
    => MonadGitHub (ActualGitHub m) where
    runGitHub req = do
        logDebug $ "GitHub request: " <> display (displayGitHubRequest req)
        auth <- OAuth . encodeUtf8 . oAccessToken <$> view optionsL
        result <- liftIO $ do
            mgr <- getGlobalManager
            executeRequestWithMgr mgr auth req
        either (throwIO . GitHubError (displayGitHubRequest req)) pure result
