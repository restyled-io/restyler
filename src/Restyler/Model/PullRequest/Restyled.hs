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
import qualified Restyler.Content as Content
import Restyler.Model.PullRequest
import Restyler.Model.PullRequestSpec
import Restyler.Model.Restyler

-- | Commit and push to the (new) restyled branch, and open a PR for it
createRestyledPullRequest
    :: (HasCallStack, MonadApp m) => [Restyler] -> m PullRequest
createRestyledPullRequest restylers = do
    pullRequest <- asks appPullRequest

    checkoutNewBranch $ pullRequestRestyledRef pullRequest
    commitAll Content.commitMessage

    -- N.B. we always force-push. There are various edge-cases that could mean
    -- an "-restyled" branch already exists and 99% of the time we can be sure
    -- it's ours. Force-pushing doesn't hurt when it's not needed (provided we
    -- know it's our branch, of course).
    forcePushOrigin $ pullRequestRestyledRef pullRequest

    let restyledTitle = pullRequestTitle pullRequest <> " (Restyled)"
        restyledBody = Content.pullRequestBody pullRequest restylers

    pr <- runGitHub $ createPullRequestR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        CreatePullRequest
            { createPullRequestTitle = restyledTitle
            , createPullRequestBody = restyledBody
            , createPullRequestHead = pullRequestRestyledRef pullRequest
            , createPullRequestBase = pullRequestRestyledBase pullRequest
            }

    pr <$ logInfoN ("Opened Restyled PR " <> showSpec (pullRequestSpec pr))

-- | Commit and force-push to the (existing) restyled branch
updateRestyledPullRequest :: MonadApp m => m ()
updateRestyledPullRequest = do
    rBranch <- asks $ pullRequestRestyledRef . appPullRequest
    checkoutNewBranch rBranch
    commitAll Content.commitMessage
    forcePushOrigin rBranch

-- | Close the Restyled PR, if we know of it
closeRestyledPullRequest :: MonadApp m => m ()
closeRestyledPullRequest = do
    -- We have to use the Owner/Repo from the main PR since SimplePullRequest
    -- doesn't give us much.
    (pullRequest, mRestyledPr) <- asks
        (appPullRequest &&& appRestyledPullRequest)

    for_ mRestyledPr $ \restyledPr -> do
        let
            spec = PullRequestSpec
                { prsOwner = pullRequestOwnerName pullRequest
                , prsRepo = pullRequestRepoName pullRequest
                , prsPullRequest = simplePullRequestNumber restyledPr
                }

        logInfoN $ "Closing restyled PR: " <> showSpec spec
        runGitHub_ $ updatePullRequestR
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

        let branch = pullRequestRestyledRef pullRequest
        logInfoN $ "Deleting restyled branch: " <> branch
        callProcess "git" ["push", "origin", "--delete", unpack branch]

-- | Commit and push to current branch
updateOriginalPullRequest :: MonadApp m => m ()
updateOriginalPullRequest = do
    commitAll Content.commitMessage
    pushOrigin . pullRequestHeadRef =<< asks appPullRequest

pushOrigin :: MonadApp m => Text -> m ()
pushOrigin branch = callProcess "git" ["push", "origin", unpack branch]

commitAll :: MonadApp m => Text -> m ()
commitAll msg = callProcess "git" ["commit", "-am", unpack msg]

forcePushOrigin :: MonadApp m => Text -> m ()
forcePushOrigin branch =
    callProcess "git" ["push", "--force-with-lease", "origin", unpack branch]

checkoutNewBranch :: MonadApp m => Text -> m ()
checkoutNewBranch branch = callProcess "git" ["checkout", "-b", unpack branch]
