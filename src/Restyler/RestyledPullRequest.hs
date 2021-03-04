module Restyler.RestyledPullRequest
    ( RestyledPullRequest
    , restyledPullRequestHeadRef
    , restyledPullRequestHtmlUrl
    , findRestyledPullRequest
    , createRestyledPullRequest
    , updateRestyledPullRequest
    , closeRestyledPullRequest
    ) where

import Restyler.Prelude

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Set as Set
import GitHub.Endpoints.Issues.Labels (addLabelsToIssueR)
import GitHub.Endpoints.PullRequests
    ( CreatePullRequest(..)
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
    )
import GitHub.Endpoints.PullRequests.ReviewRequests
    (createReviewRequestR, requestOneReviewer)
import Restyler.Capabilities.Git
import Restyler.Capabilities.GitHub
import Restyler.Capabilities.Logger
import Restyler.Comment (leaveRestyledComment)
import Restyler.Config
import qualified Restyler.Content as Content
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

instance IsPullRequest RestyledPullRequest where
    pullRequestOwnerName = restyledPullRequestOwnerName
    pullRequestRepoName = restyledPullRequestRepoName
    pullRequestNumber = restyledPullRequestNumber
    pullRequestState = restyledPullRequestState
    pullRequestHeadRef = restyledPullRequestHeadRef
    pullRequestHtmlUrl = restyledPullRequestHtmlUrl

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

findRestyledPullRequest
    :: (MonadGitHub m, MonadReader env m, HasPullRequest env)
    => m (Maybe RestyledPullRequest)
findRestyledPullRequest = do
    pullRequest <- view pullRequestL
    let ref = pullRequestRestyledHeadRef pullRequest
        legacyRef = pullRequestLocalHeadRef pullRequest <> "-restyled"
        findExisting r = existingRestyledPullRequest pullRequest r
            <$> MaybeT (findSiblingPullRequest pullRequest r)

    runMaybeT $ findExisting ref <|> findExisting legacyRef

createRestyledPullRequest
    :: ( MonadLogger m
       , MonadGitHub m
       , MonadGit m
       , MonadReader env m
       , HasOptions env
       , HasConfig env
       )
    => PullRequest
    -> [RestylerResult]
    -> m RestyledPullRequest
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

updateRestyledPullRequest
    :: MonadGit m
    => RestyledPullRequest
    -> [RestylerResult]
    -> m RestyledPullRequest
updateRestyledPullRequest restyledPullRequest _results = do
    gitPushForce $ unpack $ restyledPullRequestHeadRef restyledPullRequest
    pure restyledPullRequest

closeRestyledPullRequest :: MonadGitHub m => RestyledPullRequest -> m ()
closeRestyledPullRequest pr = do
    editPullRequestState StateClosed pr
    deleteHeadRef pr

findSiblingPullRequest
    :: MonadGitHub m => PullRequest -> Text -> m (Maybe SimplePullRequest)
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
