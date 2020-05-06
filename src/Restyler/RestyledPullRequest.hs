module Restyler.RestyledPullRequest
    ( RestyledPullRequest
    , restyledPullRequestHeadRef
    , restyledPullRequestHtmlUrl
    , HasRestyledPullRequest(..)
    , findRestyledPullRequest
    , createRestyledPullRequest
    , updateRestyledPullRequest
    , closeRestyledPullRequest
    )
where

import Restyler.Prelude

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Set as Set
import GitHub.Endpoints.GitData.References.Delete (deleteReferenceR)
import GitHub.Endpoints.Issues.Labels (addLabelsToIssueR)
import GitHub.Endpoints.PullRequests
    ( CreatePullRequest(..)
    , EditPullRequest(..)
    , Issue
    , IssueNumber
    , IssueState(..)
    , Owner
    , Repo
    , SimplePullRequest(..)
    , createPullRequestR
    , optionsBase
    , optionsHead
    , pullRequestsForR
    , toPathPart
    , unIssueNumber
    , updatePullRequestR
    )
import GitHub.Endpoints.PullRequests.ReviewRequests
    (createReviewRequestR, requestOneReviewer)
import Restyler.App.Class (HasGitHub, runGitHub, runGitHubFirst, runGitHub_)
import Restyler.App.Error (warnIgnore)
import Restyler.Comment (leaveRestyledComment)
import Restyler.Config
import qualified Restyler.Content as Content
import Restyler.Git (HasGit, gitPushForce)
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequestSpec
import Restyler.RestylerResult

data RestyledPullRequest = RestyledPullRequest
    { restyledPullRequestOwnerName :: Name Owner
    , restyledPullRequestRepoName :: Name Repo
    , restyledPullRequestNumber :: IssueNumber
    , restyledPullRequestState :: IssueState
    , restyledPullRequestHeadRef :: Text
    , restyledPullRequestHtmlUrl :: URL
    }

restyledPullRequestIssueId :: RestyledPullRequest -> Id Issue
restyledPullRequestIssueId =
    mkId Proxy . unIssueNumber . restyledPullRequestNumber

existingRestyledPullRequest
    :: PullRequest -- ^ Original PR
    -> Text -- ^ Head ref used to find the Restyled PR
    -> SimplePullRequest -- ^ Found Restyled PR
    -> RestyledPullRequest
existingRestyledPullRequest pullRequest ref simplePullRequest =
    RestyledPullRequest
        { restyledPullRequestOwnerName = pullRequestOwnerName pullRequest
        , restyledPullRequestRepoName = pullRequestRepoName pullRequest
        , restyledPullRequestNumber = simplePullRequestNumber simplePullRequest
        , restyledPullRequestState = simplePullRequestState simplePullRequest
        , restyledPullRequestHeadRef = ref
        , restyledPullRequestHtmlUrl = simplePullRequestHtmlUrl
            simplePullRequest
        }

createdRestyledPullRequest
    :: PullRequest -- ^ Created Restyled PR
    -> RestyledPullRequest
createdRestyledPullRequest restyledPullRequest = RestyledPullRequest
    { restyledPullRequestOwnerName = pullRequestOwnerName restyledPullRequest
    , restyledPullRequestRepoName = pullRequestRepoName restyledPullRequest
    , restyledPullRequestNumber = pullRequestNumber restyledPullRequest
    , restyledPullRequestState = pullRequestState restyledPullRequest
    , restyledPullRequestHeadRef = pullRequestHeadRef restyledPullRequest
    , restyledPullRequestHtmlUrl = pullRequestHtmlUrl restyledPullRequest
    }

instance Display RestyledPullRequest where
    textDisplay restyledPullRequest = textDisplay PullRequestSpec
        { prsOwner = restyledPullRequestOwnerName restyledPullRequest
        , prsRepo = restyledPullRequestRepoName restyledPullRequest
        , prsPullRequest = restyledPullRequestNumber restyledPullRequest
        }

class HasRestyledPullRequest env where
    restyledPullRequestL :: Lens' env (Maybe RestyledPullRequest)

findRestyledPullRequest
    :: HasGitHub env => PullRequest -> RIO env (Maybe RestyledPullRequest)
findRestyledPullRequest pullRequest =
    runMaybeT $ findExisting ref <|> findExisting legacyRef
  where
    ref = pullRequestRestyledHeadRef pullRequest
    legacyRef = pullRequestLocalHeadRef pullRequest <> "-restyled"

    findExisting r = existingRestyledPullRequest pullRequest r
        <$> MaybeT (findSiblingPullRequest pullRequest r)

createRestyledPullRequest
    :: ( HasLogFunc env
       , HasOptions env
       , HasConfig env
       , HasGit env
       , HasGitHub env
       )
    => PullRequest
    -> [RestylerResult]
    -> RIO env RestyledPullRequest
createRestyledPullRequest pullRequest results = do
    gitPushForce $ unpack $ pullRequestRestyledHeadRef pullRequest

    mJobUrl <- oJobUrl <$> view optionsL

    let restyledTitle = "Restyle " <> pullRequestTitle pullRequest
        restyledBody =
            Content.pullRequestDescription mJobUrl pullRequest results

    logInfo "Creating Restyled PR"
    restyledPullRequest <-
        fmap createdRestyledPullRequest $ runGitHub $ createPullRequestR
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)
            CreatePullRequest
                { createPullRequestTitle = restyledTitle
                , createPullRequestBody = restyledBody
                , createPullRequestBase = pullRequestRestyledBaseRef pullRequest
                , createPullRequestHead = pullRequestRestyledHeadRef pullRequest
                }

    whenConfigNonEmpty (Set.toList . cLabels) $ \labels -> do
        logInfo $ "Adding labels to Restyled PR (" <> displayShow labels <> ")"
        runGitHub_ $ addLabelsToIssueR
            (restyledPullRequestOwnerName restyledPullRequest)
            (restyledPullRequestRepoName restyledPullRequest)
            (restyledPullRequestIssueId restyledPullRequest)
            labels

    whenConfigJust (configPullRequestReviewer pullRequest) $ \user -> do
        logInfo $ "Requesting review of Restyled PR from " <> displayShow user
        runGitHub_ $ createReviewRequestR
            (restyledPullRequestOwnerName restyledPullRequest)
            (restyledPullRequestRepoName restyledPullRequest)
            (restyledPullRequestNumber restyledPullRequest)
            (requestOneReviewer user)

    whenConfig cComments $ do
        logInfo "Leaving comment of Restyled PR"
        leaveRestyledComment pullRequest
            $ restyledPullRequestNumber restyledPullRequest

    restyledPullRequest
        <$ logInfo ("Opened Restyled PR " <> display restyledPullRequest)

-- |
--
-- TODO: consider using results to update PR description.
--
updateRestyledPullRequest
    :: HasGit env
    => RestyledPullRequest
    -> [RestylerResult]
    -> RIO env RestyledPullRequest
updateRestyledPullRequest restyledPullRequest _results = do
    gitPushForce $ unpack $ restyledPullRequestHeadRef restyledPullRequest
    pure restyledPullRequest

closeRestyledPullRequest
    :: (HasLogFunc env, HasGitHub env) => RestyledPullRequest -> RIO env ()
closeRestyledPullRequest pr = do
    editRestyledPullRequestState StateClosed pr

    handleAny warnIgnore $ runGitHub_ $ deleteReferenceR
        (restyledPullRequestOwnerName pr)
        (restyledPullRequestRepoName pr)
        (mkName Proxy $ "heads/" <> restyledPullRequestHeadRef pr)

editRestyledPullRequestState
    :: (HasLogFunc env, HasGitHub env)
    => IssueState
    -> RestyledPullRequest
    -> RIO env ()
editRestyledPullRequestState state pr
    | restyledPullRequestState pr == state
    = logWarn
        $ "Redundant update of Restyled PR "
        <> display pr
        <> " to "
        <> displayShow state
    | otherwise
    = do
        logInfo
            $ "Updating Restyled PR "
            <> display pr
            <> " to "
            <> displayShow state

        runGitHub_ $ updatePullRequestR
            (restyledPullRequestOwnerName pr)
            (restyledPullRequestRepoName pr)
            (restyledPullRequestNumber pr)
            EditPullRequest
                { editPullRequestTitle = Nothing
                , editPullRequestBody = Nothing
                , editPullRequestState = Just state
                , editPullRequestBase = Nothing
                , editPullRequestMaintainerCanModify = Nothing
                }

findSiblingPullRequest
    :: HasGitHub env => PullRequest -> Text -> RIO env (Maybe SimplePullRequest)
findSiblingPullRequest pr ref =
    runGitHubFirst
        $ pullRequestsForR owner repo
        $ optionsBase (pullRequestRestyledBaseRef pr)
        <> optionsHead qualifiedRef
  where
    owner = pullRequestOwnerName pr
    repo = pullRequestRepoName pr
    qualifiedRef = toPathPart owner <> ":" <> ref

pullRequestRestyledBaseRef :: PullRequest -> Text
pullRequestRestyledBaseRef pullRequest
    | pullRequestIsFork pullRequest = pullRequestBaseRef pullRequest
    | otherwise = pullRequestHeadRef pullRequest
