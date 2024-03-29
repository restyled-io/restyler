module Restyler.RestyledPullRequest
  ( RestyledPullRequest
  , restyledPullRequestNumber
  , restyledPullRequestHeadRef
  , restyledPullRequestHtmlUrl
  , findRestyledPullRequest
  , createRestyledPullRequest
  , updateRestyledPullRequest
  , closeRestyledPullRequest
  ) where

import Restyler.Prelude

import qualified Data.Set as Set
import qualified Data.Text as T
import GitHub.Endpoints.GitData.References (deleteReferenceR)
import GitHub.Endpoints.Issues.Labels (addLabelsToIssueR)
import GitHub.Endpoints.PullRequests
  ( CreatePullRequest (..)
  , EditPullRequest (..)
  , Issue
  , IssueNumber
  , IssueState (..)
  , Owner
  , Repo
  , SimplePullRequest (..)
  , SimpleUser (..)
  , createPullRequestR
  , optionsHead
  , pullRequestsForR
  , toPathPart
  , unIssueNumber
  , updatePullRequestR
  )
import GitHub.Endpoints.PullRequests.ReviewRequests
  ( createReviewRequestR
  , requestOneReviewer
  )
import Restyler.App.Class (MonadGitHub, runGitHub, runGitHubFirst, runGitHub_)
import Restyler.Config
import qualified Restyler.Content as Content
import Restyler.Git (MonadGit (..))
import Restyler.Options
import Restyler.PullRequest
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
  :: PullRequest
  -- ^ Original PR
  -> Text
  -- ^ Head ref used to find the Restyled PR
  -> SimplePullRequest
  -- ^ Found Restyled PR
  -> RestyledPullRequest
existingRestyledPullRequest pullRequest ref simplePullRequest =
  RestyledPullRequest
    { restyledPullRequestOwnerName = pullRequestOwnerName pullRequest
    , restyledPullRequestRepoName = pullRequestRepoName pullRequest
    , restyledPullRequestNumber = simplePullRequestNumber simplePullRequest
    , restyledPullRequestState = simplePullRequestState simplePullRequest
    , restyledPullRequestHeadRef = ref
    , restyledPullRequestHtmlUrl =
        simplePullRequestHtmlUrl
          simplePullRequest
    }

createdRestyledPullRequest
  :: PullRequest
  -- ^ Created Restyled PR
  -> RestyledPullRequest
createdRestyledPullRequest restyledPullRequest =
  RestyledPullRequest
    { restyledPullRequestOwnerName = pullRequestOwnerName restyledPullRequest
    , restyledPullRequestRepoName = pullRequestRepoName restyledPullRequest
    , restyledPullRequestNumber = pullRequestNumber restyledPullRequest
    , restyledPullRequestState = pullRequestState restyledPullRequest
    , restyledPullRequestHeadRef = pullRequestHeadRef restyledPullRequest
    , restyledPullRequestHtmlUrl = pullRequestHtmlUrl restyledPullRequest
    }

findRestyledPullRequest
  :: MonadGitHub m => PullRequest -> m (Maybe RestyledPullRequest)
findRestyledPullRequest pullRequest =
  runMaybeT $ findExisting ref <|> findExisting legacyRef
 where
  ref = pullRequestRestyledHeadRef pullRequest
  legacyRef = pullRequestHeadRef pullRequest <> "-restyled"

  findExisting r = do
    pr <- MaybeT $ findSiblingPullRequest pullRequest r
    guard $ openedByUs pr
    pure $ existingRestyledPullRequest pullRequest r pr

  openedByUs =
    ("restyled-io" `T.isPrefixOf`)
      . untagName
      . simpleUserLogin
      . simplePullRequestUser

createRestyledPullRequest
  :: ( MonadLogger m
     , MonadGit m
     , MonadGitHub m
     , MonadReader env m
     , HasOptions env
     , HasConfig env
     )
  => PullRequest
  -> [RestylerResult]
  -> m RestyledPullRequest
createRestyledPullRequest pullRequest results = do
  gitCheckout $ unpack $ pullRequestRestyledHeadRef pullRequest
  gitPushForce $ unpack $ pullRequestRestyledHeadRef pullRequest

  mJobUrl <- oJobUrl <$> view optionsL

  let
    restyledTitle = "Restyle " <> pullRequestTitle pullRequest
    restyledBody =
      Content.pullRequestDescription mJobUrl pullRequest results

  logInfo "Creating Restyled PR"
  restyledPullRequest <-
    fmap createdRestyledPullRequest
      $ runGitHub
      $ createPullRequestR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        CreatePullRequest
          { createPullRequestTitle = restyledTitle
          , createPullRequestBody = restyledBody
          , createPullRequestBase = pullRequestRestyledBaseRef pullRequest
          , createPullRequestHead = pullRequestRestyledHeadRef pullRequest
          }

  whenConfigNonEmpty (Set.toList . cLabels) $ \labels -> do
    logInfo $ "Adding labels to Restyled PR" :# ["labels" .= labels]
    runGitHub_
      $ addLabelsToIssueR
        (restyledPullRequestOwnerName restyledPullRequest)
        (restyledPullRequestRepoName restyledPullRequest)
        (restyledPullRequestIssueId restyledPullRequest)
        labels

  whenConfigJust (configPullRequestReviewer pullRequest) $ \user -> do
    logInfo $ "Requesting review of Restyled PR" :# ["reviewer" .= user]
    runGitHub_
      $ createReviewRequestR
        (restyledPullRequestOwnerName restyledPullRequest)
        (restyledPullRequestRepoName restyledPullRequest)
        (restyledPullRequestNumber restyledPullRequest)
        (requestOneReviewer user)

  logInfo
    $ "Opened Restyled PR"
    :# ["number" .= restyledPullRequestNumber restyledPullRequest]
  pure restyledPullRequest

updateRestyledPullRequest
  :: ( MonadLogger m
     , MonadGit m
     , MonadGitHub m
     , MonadReader env m
     , HasOptions env
     )
  => PullRequest
  -> RestyledPullRequest
  -> [RestylerResult]
  -> m RestyledPullRequest
updateRestyledPullRequest pullRequest restyledPullRequest results = do
  gitCheckout $ unpack $ restyledPullRequestHeadRef restyledPullRequest
  gitPushForce $ unpack $ restyledPullRequestHeadRef restyledPullRequest

  mJobUrl <- oJobUrl <$> view optionsL
  editRestyledPullRequest restyledPullRequest $ \edit ->
    edit
      { editPullRequestBody =
          Just
            $ Content.pullRequestDescription mJobUrl pullRequest results
      }

  logInfo
    $ "Updated existing Restyled PR"
    :# ["number" .= restyledPullRequestNumber restyledPullRequest]
  pure restyledPullRequest

closeRestyledPullRequest
  :: (MonadUnliftIO m, MonadLogger m, MonadGitHub m)
  => RestyledPullRequest
  -> m ()
closeRestyledPullRequest pr = do
  logInfo
    $ "Closing existing Restyled PR"
    :# ["number" .= restyledPullRequestNumber pr]
  editRestyledPullRequestState StateClosed pr

  warnIgnore
    $ runGitHub_
    $ deleteReferenceR
      (restyledPullRequestOwnerName pr)
      (restyledPullRequestRepoName pr)
      (mkName Proxy $ "heads/" <> restyledPullRequestHeadRef pr)

editRestyledPullRequestState
  :: (MonadLogger m, MonadGitHub m)
  => IssueState
  -> RestyledPullRequest
  -> m ()
editRestyledPullRequestState issueState pr
  | restyledPullRequestState pr == issueState =
      logWarn
        $ "Redundant update of Restyled PR"
        :# ["number" .= restyledPullRequestNumber pr, "state" .= issueState]
  | otherwise =
      editRestyledPullRequest pr
        $ \edit -> edit {editPullRequestState = Just issueState}

editRestyledPullRequest
  :: MonadGitHub m
  => RestyledPullRequest
  -> (EditPullRequest -> EditPullRequest)
  -> m ()
editRestyledPullRequest pr modEdit =
  runGitHub_
    $ updatePullRequestR
      (restyledPullRequestOwnerName pr)
      (restyledPullRequestRepoName pr)
      (restyledPullRequestNumber pr)
    $ modEdit
    $ EditPullRequest
      { editPullRequestTitle = Nothing
      , editPullRequestBody = Nothing
      , editPullRequestState = Nothing
      , editPullRequestBase = Nothing
      , editPullRequestMaintainerCanModify = Nothing
      }

findSiblingPullRequest
  :: MonadGitHub m => PullRequest -> Text -> m (Maybe SimplePullRequest)
findSiblingPullRequest pr ref =
  runGitHubFirst $ pullRequestsForR owner repo $ optionsHead qualifiedRef
 where
  owner = pullRequestOwnerName pr
  repo = pullRequestRepoName pr
  qualifiedRef = toPathPart owner <> ":" <> ref
