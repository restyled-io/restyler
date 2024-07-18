module Restyler.RestyledPullRequest
  ( Restyled (..)
  , findRestyledPullRequest
  , closeRestyledPullRequest
  , RestyledPullRequestDetails (..)
  , restyledPullRequestDetails
  , createRestyledPullRequest
  , updateRestyledPullRequest
  ) where

import Restyler.Prelude

import GitHub qualified
import Restyler.Config
import Restyler.Config.RequestReview
import Restyler.Content qualified as Content
import Restyler.Git
import Restyler.GitHub.Api
import Restyler.GitHub.PullRequest
import Restyler.Options.Repository
import Restyler.RestylerResult

newtype Restyled pr = Restyled
  { unwrap :: pr
  }
  deriving newtype (HasHtmlUrl, HasNumber)

findRestyledPullRequest :: PullRequest -> m (Maybe (Restyled PullRequest))
findRestyledPullRequest = error "TODO"

closeRestyledPullRequest :: Restyled PullRequest -> m ()
closeRestyledPullRequest = error "TODO"

data RestyledPullRequestDetails = RestyledPullRequestDetails
  { repo :: RepositoryOption
  , title :: Text
  , body :: Text
  , base :: Text
  , head :: Text
  , labels :: Maybe (NonEmpty Text)
  , reviewers :: Maybe (NonEmpty Text)
  , teamReviewers :: Maybe (NonEmpty Text)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

restyledPullRequestDetails
  :: Config
  -> PullRequest
  -> [RestylerResult]
  -> RestyledPullRequestDetails
restyledPullRequestDetails config pr results =
  RestyledPullRequestDetails
    { repo =
        RepositoryOption
          { owner = pr.base.repo.owner.login
          , repo = pr.base.repo.name
          }
    , title = "Restyle " <> pr.title
    , body =
        Content.pullRequestDescription
          Nothing
          pr.number
          results
    , base = pr.head.ref
    , head = "restyled/" <> pr.head.ref
    , labels = nonEmpty $ map GitHub.untagName $ toList $ cLabels config
    , reviewers =
        pure . GitHub.untagName <$> determineReviewer pr (cRequestReview config)
    , teamReviewers = Nothing
    }

createRestyledPullRequest
  :: (MonadIO m, MonadLogger m, MonadGit m, MonadGitHub m)
  => Config
  -> PullRequest
  -> [RestylerResult]
  -> m (Restyled PullRequest)
createRestyledPullRequest config pr results = do
  gitCheckout $ unpack details.head
  gitPushForce $ unpack details.head
  logInfo $ "Creating Restyled PR" :# objectToPairs details
  restyledPullRequest <-
    Restyled
      <$> createPullRequest
        details.repo
        details.title
        details.body
        details.base
        details.head

  for_ details.labels $ \labels -> do
    logInfo $ "Adding labels to Restyled PR" :# ["labels" .= labels]
  -- runGitHub_
  --   $ addLabelsToIssueR
  --     (restyledPullRequestOwnerName restyledPullRequest)
  --     (restyledPullRequestRepoName restyledPullRequest)
  --     (restyledPullRequestIssueId restyledPullRequest)
  --     labels

  case (details.reviewers, details.teamReviewers) of
    (Nothing, Nothing) -> pure ()
    (Nothing, Just teams) -> createReviewRequest pr $ That teams
    (Just users, Nothing) -> createReviewRequest pr $ This users
    (Just users, Just teams) -> createReviewRequest pr $ These users teams

  logInfo
    $ "Opened Restyled PR"
    :# ["number" .= getNumber restyledPullRequest]
  pure restyledPullRequest
 where
  details = restyledPullRequestDetails config pr results

updateRestyledPullRequest
  :: PullRequest
  -> Restyled PullRequest
  -> [RestylerResult]
  -> m (Restyled PullRequest)
updateRestyledPullRequest = error "TODO"

-- findRestyledPullRequest
--   :: MonadGitHub m => PullRequest -> m (Maybe RestyledPullRequest)
-- findRestyledPullRequest pullRequest =
--   runMaybeT $ findExisting ref <|> findExisting legacyRef
--  where
--   ref = pullRequestRestyledHeadRef pullRequest
--   legacyRef = pullRequestHeadRef pullRequest <> "-restyled"

--   findExisting r = do
--     pr <- MaybeT $ findSiblingPullRequest pullRequest r
--     guard $ openedByUs pr
--     pure $ existingRestyledPullRequest pullRequest r pr

--   openedByUs =
--     ("restyled-io" `T.isPrefixOf`)
--       . untagName
--       . simpleUserLogin
--       . simplePullRequestUser

-- updateRestyledPullRequest
--   :: ( MonadLogger m
--      , MonadGit m
--      , MonadGitHub m
--      , MonadReader env m
--      , HasOptions env
--      )
--   => PullRequest
--   -> RestyledPullRequest
--   -> [RestylerResult]
--   -> m RestyledPullRequest
-- updateRestyledPullRequest pullRequest restyledPullRequest results = do
--   gitCheckout $ unpack $ restyledPullRequestHeadRef restyledPullRequest
--   gitPushForce $ unpack $ restyledPullRequestHeadRef restyledPullRequest

--   mJobUrl <- oJobUrl <$> view optionsL
--   editRestyledPullRequest restyledPullRequest $ \edit ->
--     edit
--       { editPullRequestBody =
--           Just
--             $ Content.pullRequestDescription
--               mJobUrl
--               (unIssueNumber $ pullRequestNumber pullRequest)
--               results
--       }

--   logInfo
--     $ "Updated existing Restyled PR"
--     :# ["number" .= restyledPullRequestNumber restyledPullRequest]
--   pure restyledPullRequest

-- closeRestyledPullRequest
--   :: (MonadUnliftIO m, MonadLogger m, MonadGitHub m)
--   => RestyledPullRequest
--   -> m ()
-- closeRestyledPullRequest pr = do
--   logInfo
--     $ "Closing existing Restyled PR"
--     :# ["number" .= restyledPullRequestNumber pr]
--   editRestyledPullRequestState StateClosed pr

--   warnIgnore
--     $ runGitHub_
--     $ deleteReferenceR
--       (restyledPullRequestOwnerName pr)
--       (restyledPullRequestRepoName pr)
--       (mkName Proxy $ "heads/" <> restyledPullRequestHeadRef pr)

-- editRestyledPullRequestState
--   :: (MonadLogger m, MonadGitHub m)
--   => IssueState
--   -> RestyledPullRequest
--   -> m ()
-- editRestyledPullRequestState issueState pr
--   | restyledPullRequestState pr == issueState =
--       logWarn
--         $ "Redundant update of Restyled PR"
--         :# ["number" .= restyledPullRequestNumber pr, "state" .= issueState]
--   | otherwise =
--       editRestyledPullRequest pr
--         $ \edit -> edit {editPullRequestState = Just issueState}

-- editRestyledPullRequest
--   :: MonadGitHub m
--   => RestyledPullRequest
--   -> (EditPullRequest -> EditPullRequest)
--   -> m ()
-- editRestyledPullRequest pr modEdit =
--   runGitHub_
--     $ updatePullRequestR
--       (restyledPullRequestOwnerName pr)
--       (restyledPullRequestRepoName pr)
--       (restyledPullRequestNumber pr)
--     $ modEdit
--     $ EditPullRequest
--       { editPullRequestTitle = Nothing
--       , editPullRequestBody = Nothing
--       , editPullRequestState = Nothing
--       , editPullRequestBase = Nothing
--       , editPullRequestMaintainerCanModify = Nothing
--       }

-- findSiblingPullRequest
--   :: MonadGitHub m => PullRequest -> Text -> m (Maybe SimplePullRequest)
-- findSiblingPullRequest pr ref =
--   runGitHubFirst $ pullRequestsForR owner repo $ optionsHead qualifiedRef
--  where
--   owner = pullRequestOwnerName pr
--   repo = pullRequestRepoName pr
--   qualifiedRef = toPathPart owner <> ":" <> ref
