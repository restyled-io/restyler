{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Restyler.Model.PullRequest.Restyled
    ( createRestyledPullRequest
    , updateRestyledPullRequest
    , closeRestyledPullRequest
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

-- | Close the Restyled PR, if we know of it
--
-- TODO: delete the branch
--
closeRestyledPullRequest
    :: (MonadGitHub m, MonadLogger m, MonadReader App m) => m ()
closeRestyledPullRequest = do
    -- We have to use the Owner/Repo from the main PR since SimplePullRequest
    -- doesn't give us much.
    (pullRequest, mRestyledPr) <- asks
        (appPullRequest &&& appRestyledPullRequest)

    for_ mRestyledPr $ \restyledPr -> do
        let spec = PullRequestSpec
                { prsOwner = pullRequestOwnerName pullRequest
                , prsRepo = pullRequestRepoName pullRequest
                , prsPullRequest = simplePullRequestNumber restyledPr
                }
        logInfoN $ "Closing restyled PR: " <> showSpec spec

        updatePullRequest
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)
            (mkId Proxy $ simplePullRequestNumber restyledPr)
            EditPullRequest
                { editPullRequestTitle = Nothing
                , editPullRequestBody = Nothing
                , editPullRequestState = Just StateClosed
                , editPullRequestBase = Nothing
                , editPullRequestMaintainerCanModify = Nothing
                }

-- | Commit and push to current branch
updateOriginalPullRequest :: (MonadGit m, MonadReader App m) => m ()
updateOriginalPullRequest = do
    commitAll Content.commitMessage
    pushOrigin . pullRequestHeadRef =<< asks appPullRequest
