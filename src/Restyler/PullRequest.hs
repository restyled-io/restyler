module Restyler.PullRequest
  ( PullRequest
  , pullRequestHtmlUrl
  , pullRequestNumber
  , pullRequestTitle
  , pullRequestState
  , HasPullRequest (..)
  , pullRequestOwnerName
  , pullRequestRepoName
  , pullRequestRepoPublic
  , pullRequestUserLogin
  , pullRequestCloneUrlToken
  , pullRequestIssueId
  , pullRequestIsClosed
  , pullRequestIsFork
  , pullRequestBaseRef
  , pullRequestHeadRef
  , pullRequestHeadSha
  , pullRequestRemoteHeadRef
  , pullRequestLocalHeadRef
  , pullRequestRestyledBaseRef
  , pullRequestRestyledHeadRef
  ) where

import Restyler.Prelude

import GitHub.Data

class HasPullRequest env where
  pullRequestL :: Lens' env PullRequest

pullRequestOwnerName :: HasCallStack => PullRequest -> Name Owner
pullRequestOwnerName = simpleOwnerLogin . pullRequestOwner

pullRequestRepoName :: HasCallStack => PullRequest -> Name Repo
pullRequestRepoName = repoName . pullRequestRepo

pullRequestRepoPublic :: HasCallStack => PullRequest -> Bool
pullRequestRepoPublic = not . repoPrivate . pullRequestRepo

pullRequestUserLogin :: PullRequest -> Name User
pullRequestUserLogin = simpleUserLogin . pullRequestUser

-- | Clone URL using the given Access Token
pullRequestCloneUrlToken :: HasCallStack => Text -> PullRequest -> Text
pullRequestCloneUrlToken token pullRequest =
  "https://x-access-token:"
    <> token
    <> "@github.com/"
    <> untagName (pullRequestOwnerName pullRequest)
    <> "/"
    <> untagName (pullRequestRepoName pullRequest)
    <> ".git"

-- | Some API actions need to treat the PR like an Issue
pullRequestIssueId :: PullRequest -> Id Issue
pullRequestIssueId = mkId Proxy . unIssueNumber . pullRequestNumber

pullRequestIsClosed :: PullRequest -> Bool
pullRequestIsClosed = (== StateClosed) . pullRequestState

pullRequestIsFork :: PullRequest -> Bool
pullRequestIsFork = (/=) <$> pullRequestHeadRepo <*> pullRequestBaseRepo

pullRequestBaseRef :: PullRequest -> Text
pullRequestBaseRef = pullRequestCommitRef . pullRequestBase

pullRequestHeadRef :: PullRequest -> Text
pullRequestHeadRef = pullRequestCommitRef . pullRequestHead

pullRequestHeadSha :: PullRequest -> Text
pullRequestHeadSha = pullRequestCommitSha . pullRequestHead

pullRequestRemoteHeadRef :: PullRequest -> Text
pullRequestRemoteHeadRef PullRequest {..} =
  "pull/" <> toPathPart pullRequestNumber <> "/head"

pullRequestLocalHeadRef :: PullRequest -> Text
pullRequestLocalHeadRef PullRequest {..} =
  "pull-" <> toPathPart pullRequestNumber

pullRequestRestyledBaseRef :: PullRequest -> Text
pullRequestRestyledBaseRef pullRequest
  | pullRequestIsFork pullRequest = pullRequestBaseRef pullRequest
  | otherwise = pullRequestHeadRef pullRequest

pullRequestRestyledHeadRef :: PullRequest -> Text
pullRequestRestyledHeadRef = ("restyled/" <>) . pullRequestHeadRef

--------------------------------------------------------------------------------
-- Internal functions below this point
--------------------------------------------------------------------------------

pullRequestOwner :: HasCallStack => PullRequest -> SimpleOwner
pullRequestOwner = repoOwner . pullRequestRepo

-- |
--
-- N.B. The source of all partiality and @'HasCallStack'@ constraints
pullRequestRepo :: HasCallStack => PullRequest -> Repo
pullRequestRepo =
  fromMaybe (error "Pull Request without Repository") . pullRequestBaseRepo

pullRequestBaseRepo :: PullRequest -> Maybe Repo
pullRequestBaseRepo = pullRequestCommitRepo . pullRequestBase

pullRequestHeadRepo :: PullRequest -> Maybe Repo
pullRequestHeadRepo = pullRequestCommitRepo . pullRequestHead
