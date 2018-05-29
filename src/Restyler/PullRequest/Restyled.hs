{-# LANGUAGE OverloadedStrings #-}

module Restyler.PullRequest.Restyled
    ( restyledPullRequestExists
    , createRestyledPullRequest
    , updateRestyledPullRequest
    , updateOriginalPullRequest
    )
where

import Restyler.Prelude

import GitHub
import qualified Restyler.Content as Content
import Restyler.Git
import Restyler.GitHub
import Restyler.PullRequest
import Restyler.RepoSpec

-- | See if a branch exists with our name and message
restyledPullRequestExists :: AppM Bool
restyledPullRequestExists = do
    rBranch <- asks $ pullRequestRestyledRef . appPullRequest
    (== Just Content.commitMessage) <$> branchHeadMessage ("origin/" <> rBranch)

-- | Commit and push to the (new) restyled branch, and open a PR for it
createRestyledPullRequest :: AppM PullRequest
createRestyledPullRequest = do
    pullRequest <- asks appPullRequest
    let rBranch = pullRequestRestyledRef pullRequest

    checkoutBranch True rBranch
    commitAll Content.commitMessage
    pushOrigin $ pullRequestRestyledRef pullRequest

    pr <- runGitHub $ createPullRequestR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        CreatePullRequest
            { createPullRequestTitle = pullRequestTitle pullRequest
                <> " (Restyled)"
            , createPullRequestBody = ""
            , createPullRequestHead = pullRequestRestyledRef pullRequest
            , createPullRequestBase = if pullRequestIsFork pullRequest
                then pullRequestBaseRef pullRequest
                else pullRequestHeadRef pullRequest
            }

    pr <$ logInfoN
        ("Opened Restyled PR " <> showRepoSpec (pullRequestRepoSpec pr))

-- | Commit and force-push to the (existing) restyled branch
updateRestyledPullRequest :: AppM ()
updateRestyledPullRequest = do
    rBranch <- asks $ pullRequestRestyledRef . appPullRequest
    checkoutBranch True rBranch
    commitAll Content.commitMessage
    forcePushOrigin rBranch

-- | Commit and push to current branch
updateOriginalPullRequest :: AppM ()
updateOriginalPullRequest = do
    commitAll Content.commitMessage
    pushOrigin . pullRequestHeadRef =<< asks appPullRequest
