{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyler.PullRequest
    ( PullRequest
    , pullRequestHtmlUrl
    , pullRequestNumber
    , pullRequestTitle
    , pullRequestState
    , HasPullRequest(..)
    , pullRequestOwnerName
    , pullRequestRepoName
    , pullRequestUserLogin
    , pullRequestCloneUrl
    , pullRequestCloneUrlToken
    , pullRequestIssueId
    , pullRequestIsClosed
    , pullRequestIsFork
    , pullRequestIsNonDefaultBranch
    , pullRequestBaseRef
    , pullRequestHeadRef
    , pullRequestHeadSha
    , pullRequestRemoteHeadRef
    , pullRequestLocalHeadRef
    , pullRequestRestyledHeadRef
    ) where

import Restyler.Prelude

import GitHub.Data
import Restyler.PullRequestSpec

instance Display PullRequest where
    textDisplay pullRequest = textDisplay PullRequestSpec
        { prsOwner = pullRequestOwnerName pullRequest
        , prsRepo = pullRequestRepoName pullRequest
        , prsPullRequest = pullRequestNumber pullRequest
        }

class HasPullRequest env where
    pullRequestL :: Lens' env PullRequest

pullRequestOwnerName :: HasCallStack => PullRequest -> Name Owner
pullRequestOwnerName = simpleOwnerLogin . pullRequestOwner

pullRequestRepoName :: HasCallStack => PullRequest -> Name Repo
pullRequestRepoName = repoName . pullRequestRepo

pullRequestUserLogin :: PullRequest -> Name User
pullRequestUserLogin = simpleUserLogin . pullRequestUser

-- | Clone URL appropriate to output in a message
--
-- This is a URL that will work if you are otherwised authorized to clone the
-- repository (e.g.) you have an SSH key.
--
pullRequestCloneUrl :: HasCallStack => PullRequest -> URL
pullRequestCloneUrl =
    fromJustNote "Pull Request without clone URL"
        . repoCloneUrl
        . pullRequestRepo

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

pullRequestIsNonDefaultBranch :: PullRequest -> Bool
pullRequestIsNonDefaultBranch =
    (/=) <$> pullRequestBaseRef <*> pullRequestDefaultBranch

pullRequestBaseRef :: PullRequest -> Text
pullRequestBaseRef = pullRequestCommitRef . pullRequestBase

pullRequestHeadRef :: PullRequest -> Text
pullRequestHeadRef = pullRequestCommitRef . pullRequestHead

pullRequestHeadSha :: PullRequest -> Text
pullRequestHeadSha = pullRequestCommitSha . pullRequestHead

pullRequestRemoteHeadRef :: PullRequest -> Text
pullRequestRemoteHeadRef pullRequest@PullRequest {..}
    | pullRequestIsFork pullRequest
    = "pull/" <> toPathPart pullRequestNumber <> "/head"
    | otherwise
    = pullRequestCommitRef pullRequestHead

pullRequestLocalHeadRef :: PullRequest -> Text
pullRequestLocalHeadRef pullRequest@PullRequest {..}
    | pullRequestIsFork pullRequest = "pull-" <> toPathPart pullRequestNumber
    | otherwise = pullRequestCommitRef pullRequestHead

pullRequestRestyledHeadRef :: PullRequest -> Text
pullRequestRestyledHeadRef = ("restyled/" <>) . pullRequestLocalHeadRef

--------------------------------------------------------------------------------
-- Internal functions below this point
--------------------------------------------------------------------------------

pullRequestOwner :: HasCallStack => PullRequest -> SimpleOwner
pullRequestOwner = repoOwner . pullRequestRepo

-- |
--
-- N.B. The source of all partiality and @'HasCallStack'@ constraints
--
pullRequestRepo :: HasCallStack => PullRequest -> Repo
pullRequestRepo =
    fromJustNote "Pull Request without Repository" . pullRequestBaseRepo

pullRequestBaseRepo :: PullRequest -> Maybe Repo
pullRequestBaseRepo = pullRequestCommitRepo . pullRequestBase

pullRequestHeadRepo :: PullRequest -> Maybe Repo
pullRequestHeadRepo = pullRequestCommitRepo . pullRequestHead

pullRequestDefaultBranch :: PullRequest -> Text
pullRequestDefaultBranch =
    fromMaybe "main" . (repoDefaultBranch <=< pullRequestBaseRepo)
