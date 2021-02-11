{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyler.PullRequest
    ( PullRequest
    , IsPullRequest(..)
    , pullRequestTitle
    , HasPullRequest(..)
    , pullRequestUserLogin
    , pullRequestCloneUrl
    , pullRequestCloneUrlToken
    , pullRequestIssueId
    , pullRequestIsClosed
    , pullRequestIsFork
    , pullRequestIsNonDefaultBranch
    , pullRequestBaseRef
    , pullRequestHeadSha
    , pullRequestRemoteHeadRef
    , pullRequestLocalHeadRef
    , pullRequestRestyledHeadRef
    )
where

import Restyler.Prelude

import GitHub.Data hiding
    (pullRequestHtmlUrl, pullRequestNumber, pullRequestState)
import qualified GitHub.Data as GH
import Restyler.PullRequestSpec

instance Display PullRequest where
    textDisplay pullRequest = textDisplay PullRequestSpec
        { prsOwner = pullRequestOwnerName pullRequest
        , prsRepo = pullRequestRepoName pullRequest
        , prsPullRequest = pullRequestNumber pullRequest
        }

class HasPullRequest env where
    pullRequestL :: Lens' env PullRequest

class IsPullRequest pr where
    pullRequestOwnerName :: pr -> Name Owner
    pullRequestRepoName :: pr -> Name Repo
    pullRequestNumber :: pr -> IssueNumber
    pullRequestState :: pr -> IssueState
    pullRequestHeadRef :: pr -> Text
    pullRequestHtmlUrl :: pr -> URL

instance IsPullRequest PullRequest where
    pullRequestOwnerName = simpleOwnerLogin . pullRequestOwner
    pullRequestRepoName = repoName . pullRequestRepo
    pullRequestNumber = GH.pullRequestNumber
    pullRequestState = GH.pullRequestState
    pullRequestHeadRef = pullRequestCommitRef . pullRequestHead
    pullRequestHtmlUrl = GH.pullRequestHtmlUrl

pullRequestUserLogin :: PullRequest -> Name User
pullRequestUserLogin = simpleUserLogin . pullRequestUser

-- | Clone URL appropriate to output in a message
--
-- This is a URL that will work if you are otherwised authorized to clone the
-- repository (e.g.) you have an SSH key.
--
pullRequestCloneUrl :: PullRequest -> URL
pullRequestCloneUrl =
    fromJustNote "Pull Request without clone URL"
        . repoCloneUrl
        . pullRequestRepo

-- | Clone URL using the given Access Token
pullRequestCloneUrlToken :: Text -> PullRequest -> Text
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

pullRequestHeadSha :: PullRequest -> Text
pullRequestHeadSha = pullRequestCommitSha . pullRequestHead

pullRequestRemoteHeadRef :: PullRequest -> Text
pullRequestRemoteHeadRef pr
    | pullRequestIsFork pr
    = "pull/" <> toPathPart (pullRequestNumber pr) <> "/head"
    | otherwise
    = pullRequestCommitRef $ pullRequestHead pr

pullRequestLocalHeadRef :: PullRequest -> Text
pullRequestLocalHeadRef pr
    | pullRequestIsFork pr = "pull-" <> toPathPart (pullRequestNumber pr)
    | otherwise = pullRequestCommitRef $ pullRequestHead pr

pullRequestRestyledHeadRef :: PullRequest -> Text
pullRequestRestyledHeadRef = ("restyled/" <>) . pullRequestLocalHeadRef

--------------------------------------------------------------------------------
-- Internal functions below this point
--------------------------------------------------------------------------------

pullRequestOwner :: PullRequest -> SimpleOwner
pullRequestOwner = repoOwner . pullRequestRepo

pullRequestRepo :: HasCallStack => PullRequest -> Repo
pullRequestRepo =
    fromJustNote "Pull Request without Repository" . pullRequestBaseRepo

pullRequestBaseRepo :: PullRequest -> Maybe Repo
pullRequestBaseRepo = pullRequestCommitRepo . pullRequestBase

pullRequestHeadRepo :: PullRequest -> Maybe Repo
pullRequestHeadRepo = pullRequestCommitRepo . pullRequestHead

pullRequestDefaultBranch :: PullRequest -> Text
pullRequestDefaultBranch =
    fromMaybe "master" . (repoDefaultBranch <=< pullRequestBaseRepo)
