module Restyler.PullRequest.Restyled
    ( createRestyledPullRequest
    , updateRestyledPullRequest
    , closeRestyledPullRequest
    , closeRestyledPullRequest'
    , updateOriginalPullRequest
    )
where

import Restyler.Prelude

import GitHub.Endpoints.GitData.References.Delete
import GitHub.Endpoints.Issues.Labels
import GitHub.Endpoints.PullRequests hiding (pullRequest)
import GitHub.Endpoints.PullRequests.ReviewRequests
import Restyler.App.Class
import Restyler.Config
import Restyler.Config.RequestReview
import qualified Restyler.Content as Content
import Restyler.Git
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequestSpec
import Restyler.RestylerResult

-- | Commit and push to the (new) restyled branch, and open a PR for it
createRestyledPullRequest
    :: ( HasCallStack
       , HasLogFunc env
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       , HasGit env
       , HasGitHub env
       )
    => [RestylerResult]
    -> RIO env PullRequest
createRestyledPullRequest results = do
    mJobUrl <- oJobUrl <$> view optionsL
    pullRequest <- view pullRequestL

    -- N.B. we always force-push. There are various edge-cases that could mean
    -- an "-restyled" branch already exists and 99% of the time we can be sure
    -- it's ours. Force-pushing doesn't hurt when it's not needed (provided we
    -- know it's our branch, of course).
    gitPushForce . unpack $ pullRequestRestyledRef pullRequest

    let restyledTitle = "Restyle " <> pullRequestTitle pullRequest
        restyledBody =
            Content.pullRequestDescription mJobUrl pullRequest results

    logInfo "Creating Restyled PR"
    pr <- runGitHub $ createPullRequestR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        CreatePullRequest
            { createPullRequestTitle = restyledTitle
            , createPullRequestBody = restyledBody
            , createPullRequestHead = pullRequestRestyledRef pullRequest
            , createPullRequestBase = pullRequestRestyledBase pullRequest
            }

    whenConfigNonEmpty cLabels $ \labels -> do
        logInfo $ "Adding labels to Restyled PR (" <> displayShow labels <> ")"
        runGitHub_ $ addLabelsToIssueR
            (pullRequestOwnerName pr)
            (pullRequestRepoName pr)
            (pullRequestIssueId pr)
            labels

    whenConfigJust cRequestReview $ \req -> do
        logInfo $ "Requesting review of Restyled PR (" <> displayShow req <> ")"
        runGitHub_
            $ createReviewRequestR
                  (pullRequestOwnerName pr)
                  (pullRequestRepoName pr)
                  (pullRequestNumber pr)
            $ requestOneReviewer
            $ determineReviewer req pullRequest

    pr <$ logInfo ("Opened Restyled PR " <> displayShow (pullRequestSpec pr))

-- | Commit and force-push to the (existing) restyled branch
updateRestyledPullRequest :: (HasPullRequest env, HasGit env) => RIO env ()
updateRestyledPullRequest = do
    rBranch <- pullRequestRestyledRef <$> view pullRequestL
    gitPushForce $ unpack rBranch

-- | Close the Restyled PR, if we know of it
closeRestyledPullRequest
    :: ( HasLogFunc env
       , HasPullRequest env
       , HasRestyledPullRequest env
       , HasGitHub env
       )
    => RIO env ()
closeRestyledPullRequest = do
    -- We have to use the Owner/Repo from the main PR since SimplePullRequest
    -- doesn't give us much.
    pullRequest <- view pullRequestL
    mRestyledPr <- view restyledPullRequestL

    closeRestyledPullRequest' pullRequest mRestyledPr

-- | Extracted for use during Setup (e.g. without @'HasPullRequest'@)
closeRestyledPullRequest'
    :: (HasLogFunc env, HasGitHub env)
    => PullRequest
    -> Maybe SimplePullRequest
    -> RIO env ()
closeRestyledPullRequest' pullRequest mRestyledPr =
    for_ mRestyledPr $ \restyledPr -> do
        let
            spec = PullRequestSpec
                { prsOwner = pullRequestOwnerName pullRequest
                , prsRepo = pullRequestRepoName pullRequest
                , prsPullRequest = simplePullRequestNumber restyledPr
                }

        logInfo $ "Closing restyled PR: " <> displayShow spec
        runGitHub_ $ updatePullRequestR
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)
            (simplePullRequestNumber restyledPr)
            EditPullRequest
                { editPullRequestTitle = Nothing
                , editPullRequestBody = Nothing
                , editPullRequestState = Just StateClosed
                , editPullRequestBase = Nothing
                , editPullRequestMaintainerCanModify = Nothing
                }

        let branch = pullRequestRestyledRef pullRequest
        logInfo $ "Deleting restyled branch: " <> displayShow branch
        runGitHub_ $ deleteReferenceR
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)
            (mkName Proxy $ "heads/" <> branch)

-- | Commit and push to current branch
updateOriginalPullRequest :: (HasPullRequest env, HasGit env) => RIO env ()
updateOriginalPullRequest =
    gitPush . unpack . pullRequestHeadRef =<< view pullRequestL
