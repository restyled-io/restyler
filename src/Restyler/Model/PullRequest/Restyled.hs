{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Restyler.Model.PullRequest.Restyled
    ( restyledPullRequestExists
    , createRestyledPullRequest
    , updateRestyledPullRequest
    , updateOriginalPullRequest
    )
where

import Restyler.Prelude

import Restyler.App
import Restyler.Capabilities.Git
import Restyler.Capabilities.GitHub
import qualified Restyler.Content as Content
import Restyler.Model.PullRequest
import Restyler.Model.PullRequestSpec

-- | See if a branch exists with our name and message
restyledPullRequestExists :: (MonadGit m, MonadReader App m) => m Bool
restyledPullRequestExists = do
    rBranch <- asks $ pullRequestRestyledRef . appPullRequest
    (== Just Content.commitMessage) <$> branchHeadMessage ("origin/" <> rBranch)

-- | Commit and push to the (new) restyled branch, and open a PR for it
createRestyledPullRequest
    :: (MonadGit m, MonadGitHub m, MonadLogger m, MonadReader App m)
    => m PullRequest
createRestyledPullRequest = do
    pullRequest <- asks appPullRequest
    let rBranch = pullRequestRestyledRef pullRequest

    checkoutBranch True rBranch
    commitAll Content.commitMessage
    pushOrigin $ pullRequestRestyledRef pullRequest

    pr <- createPullRequest
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        CreatePullRequest
            { createPullRequestTitle = pullRequestTitle pullRequest
                <> " (Restyled)"
            , createPullRequestBody = ""
            , createPullRequestHead = pullRequestRestyledRef pullRequest
            , createPullRequestBase = pullRequestRestyledBase pullRequest
            }

    pr <$ logInfoN ("Opened Restyled PR " <> showSpec (pullRequestSpec pr))

-- | Commit and force-push to the (existing) restyled branch
updateRestyledPullRequest :: (MonadGit m, MonadReader App m) => m ()
updateRestyledPullRequest = do
    rBranch <- asks $ pullRequestRestyledRef . appPullRequest
    checkoutBranch True rBranch
    commitAll Content.commitMessage
    forcePushOrigin rBranch

-- | Commit and push to current branch
updateOriginalPullRequest :: (MonadGit m, MonadReader App m) => m ()
updateOriginalPullRequest = do
    commitAll Content.commitMessage
    pushOrigin . pullRequestHeadRef =<< asks appPullRequest
