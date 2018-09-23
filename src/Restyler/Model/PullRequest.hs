{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Model.PullRequest
    ( pullRequestOwnerName
    , pullRequestRepoName
    , pullRequestCloneUrl
    , pullRequestCloneUrlToken
    , pullRequestSpec
    , simplePullRequestSpec
    , pullRequestIssueId
    , pullRequestIsFork
    , pullRequestIsNonDefaultBranch
    , pullRequestBaseRef
    , pullRequestHeadRef
    , pullRequestHeadSha
    , pullRequestRemoteHeadRef
    , pullRequestLocalHeadRef
    , pullRequestRestyledBase
    , pullRequestRestyledRef
    , pullRequestRestyledMod
    )
where

import Restyler.Prelude

import Restyler.Model.PullRequestSpec

pullRequestOwnerName :: HasCallStack => PullRequest -> Name Owner
pullRequestOwnerName = simpleOwnerLogin . pullRequestOwner

pullRequestRepoName :: HasCallStack => PullRequest -> Name Repo
pullRequestRepoName = repoName . pullRequestRepo

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

pullRequestSpec :: HasCallStack => PullRequest -> PullRequestSpec
pullRequestSpec pullRequest = PullRequestSpec
    { prsOwner = pullRequestOwnerName pullRequest
    , prsRepo = pullRequestRepoName pullRequest
    , prsPullRequest = pullRequestNumber pullRequest
    }

simplePullRequestSpec :: SimplePullRequest -> PullRequestSpec
simplePullRequestSpec = undefined

-- | Some API actions need to treat the PR like an Issue
pullRequestIssueId :: PullRequest -> Id Issue
pullRequestIssueId = mkId Proxy . pullRequestNumber

pullRequestIsFork :: PullRequest -> Bool
pullRequestIsFork = (/=) <$> pullRequestHeadRepo <*> pullRequestBaseRepo

pullRequestIsNonDefaultBranch :: HasCallStack => PullRequest -> Bool
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
    = "pull/" <> tshow pullRequestNumber <> "/head"
    | otherwise
    = pullRequestCommitRef pullRequestHead

pullRequestLocalHeadRef :: PullRequest -> Text
pullRequestLocalHeadRef pullRequest@PullRequest {..}
    | pullRequestIsFork pullRequest = "pull-" <> tshow pullRequestNumber
    | otherwise = pullRequestCommitRef pullRequestHead

pullRequestRestyledBase :: PullRequest -> Text
pullRequestRestyledBase pullRequest
    | pullRequestIsFork pullRequest = pullRequestBaseRef pullRequest
    | otherwise = pullRequestHeadRef pullRequest

pullRequestRestyledRef :: PullRequest -> Text
pullRequestRestyledRef = (<> "-restyled") . pullRequestLocalHeadRef

pullRequestRestyledMod :: PullRequest -> PullRequestMod
pullRequestRestyledMod pullRequest = mconcat
    [ optionsBase $ pullRequestRestyledBase pullRequest
    , optionsHead $ pullRequestRestyledRefQualified pullRequest
    ]

--------------------------------------------------------------------------------
-- Internal functions below this point
--------------------------------------------------------------------------------

pullRequestOwner :: HasCallStack => PullRequest -> SimpleOwner
pullRequestOwner = repoOwner . pullRequestRepo

pullRequestRestyledRefQualified :: HasCallStack => PullRequest -> Text
pullRequestRestyledRefQualified pullRequest =
    toPathPart (pullRequestOwnerName pullRequest)
        <> ":"
        <> pullRequestRestyledRef pullRequest

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
    fromMaybe "master" . (repoDefaultBranch <=< pullRequestBaseRepo)
