module Restyler.PullRequest.Restyled
    ( createOrUpdateRestyledPullRequest
    , closeRestyledPullRequest
    , closeRestyledPullRequest'
    , updateOriginalPullRequest
    )
where

import Restyler.Prelude

import qualified Data.Set as Set
import GitHub.Endpoints.GitData.References.Delete
import GitHub.Endpoints.Issues.Labels
import GitHub.Endpoints.PullRequests hiding (pullRequest)
import GitHub.Endpoints.PullRequests.ReviewRequests
import Restyler.App.Class
import Restyler.Comment
import Restyler.Config
import qualified Restyler.Content as Content
import Restyler.Git
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequestSpec
import Restyler.RestylerResult

createOrUpdateRestyledPullRequest
    :: ( HasLogFunc env
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       , HasGit env
       , HasGitHub env
       , HasRestyledPullRequest env
       )
    => [RestylerResult]
    -> RIO env URL
createOrUpdateRestyledPullRequest results = fromMaybeM
    (pullRequestHtmlUrl <$> createRestyledPullRequest results)
    (simplePullRequestHtmlUrl <$$> updateRestyledPullRequest)

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

    whenConfigNonEmpty (Set.toList . cLabels) $ \labels -> do
        logInfo $ "Adding labels to Restyled PR (" <> displayShow labels <> ")"
        runGitHub_ $ addLabelsToIssueR
            (pullRequestOwnerName pr)
            (pullRequestRepoName pr)
            (pullRequestIssueId pr)
            labels

    whenConfigJust (configPullRequestReviewer pullRequest) $ \user -> do
        logInfo $ "Requesting review of Restyled PR from " <> displayShow user
        runGitHub_ $ createReviewRequestR
            (pullRequestOwnerName pr)
            (pullRequestRepoName pr)
            (pullRequestNumber pr)
            (requestOneReviewer user)

    whenConfig cComments $ do
        logInfo "Leaving comment of Restyled PR"
        leaveRestyledComment pr

    pr <$ logInfo ("Opened Restyled PR " <> displayShow (pullRequestSpec pr))

updateRestyledPullRequest
    :: ( HasLogFunc env
       , HasPullRequest env
       , HasRestyledPullRequest env
       , HasGit env
       , HasGitHub env
       )
    => RIO env (Maybe SimplePullRequest)
updateRestyledPullRequest = do
    pullRequest <- view pullRequestL
    mRestyledPr <- view restyledPullRequestL

    with mRestyledPr $ \restyledPr -> do
        logInfo "Updating existing Restyle PR"

        when (simplePullRequestState restyledPr == StateClosed) $ do
            logInfo "Restyle PR was closed, re-opening"
            editRestyledPullRequest
                pullRequest
                restyledPr
                EditPullRequest
                    { editPullRequestTitle = Nothing
                    , editPullRequestBody = Nothing
                    , editPullRequestState = Just StateOpen
                    , editPullRequestBase = Nothing
                    , editPullRequestMaintainerCanModify = Nothing
                    }

        logInfo "Pushing to Restyle commits to existing branch"
        gitPushForce $ unpack $ pullRequestRestyledRef pullRequest

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
        editRestyledPullRequest
            pullRequest
            restyledPr
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

editRestyledPullRequest
    :: HasGitHub env
    => PullRequest
    -> SimplePullRequest
    -> EditPullRequest
    -> RIO env ()
editRestyledPullRequest pullRequest restyledPr =
    runGitHub_ . updatePullRequestR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (simplePullRequestNumber restyledPr)

-- | Commit and push to current branch
updateOriginalPullRequest :: (HasPullRequest env, HasGit env) => RIO env ()
updateOriginalPullRequest =
    gitPush . unpack . pullRequestHeadRef =<< view pullRequestL
