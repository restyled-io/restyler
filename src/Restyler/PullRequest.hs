{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.PullRequest
    ( pullRequestOwnerName
    , pullRequestRepoName
    , pullRequestRepoSpec
    , pullRequestRepoURL
    , pullRequestURL
    , pullRequestIssueId
    , pullRequestIsFork
    , pullRequestBaseRef
    , pullRequestHeadRef
    , pullRequestRemoteHeadRef
    , pullRequestLocalHeadRef
    , pullRequestRestyledRef
    )
where

import Restyler.Prelude

import GHC.Stack
import GitHub.Data
import Restyler.RepoSpec

pullRequestOwnerName :: PullRequest -> Name Owner
pullRequestOwnerName = simpleOwnerLogin . pullRequestOwner

pullRequestRepoName :: HasCallStack => PullRequest -> Name Repo
pullRequestRepoName = repoName . pullRequestRepo

pullRequestRepoSpec :: HasCallStack => PullRequest -> RepoSpec
pullRequestRepoSpec pullRequest = RepoSpec
    { rsOwner = pullRequestOwnerName pullRequest
    , rsRepo = pullRequestRepoName pullRequest
    , rsPullRequest = pullRequestNumber pullRequest
    }

pullRequestRepoURL :: HasCallStack => PullRequest -> Text
pullRequestRepoURL pullRequest = "https://github.com/" <> owner <> "/" <> repo
  where
    owner = untagName $ pullRequestOwnerName pullRequest
    repo = untagName $ pullRequestRepoName pullRequest

pullRequestURL :: HasCallStack => PullRequest -> Text
pullRequestURL pullRequest =
    pullRequestRepoURL pullRequest <> "/pull/" <> tshow
        (pullRequestNumber pullRequest)

-- | Some API actions need to treat the PR like an Issue
pullRequestIssueId :: PullRequest -> Id Issue
pullRequestIssueId = mkId Proxy . pullRequestNumber

-- brittany-disable-next-binding
pullRequestIsFork :: PullRequest -> Bool
pullRequestIsFork = (/=)
    <$> pullRequestHeadRepo
    <*> pullRequestBaseRepo

pullRequestBaseRef :: PullRequest -> Text
pullRequestBaseRef = pullRequestCommitRef . pullRequestBase

pullRequestHeadRef :: PullRequest -> Text
pullRequestHeadRef = pullRequestCommitRef . pullRequestHead

-- brittany-disable-next-binding
pullRequestRemoteHeadRef :: PullRequest -> Text
pullRequestRemoteHeadRef pullRequest@PullRequest {..}
    | pullRequestIsFork pullRequest =
        "pull/" <> tshow pullRequestNumber <> "/head"
    | otherwise = pullRequestCommitRef pullRequestHead

pullRequestLocalHeadRef :: PullRequest -> Text
pullRequestLocalHeadRef pullRequest@PullRequest {..}
    | pullRequestIsFork pullRequest = "pull-" <> tshow pullRequestNumber
    | otherwise = pullRequestCommitRef pullRequestHead

pullRequestRestyledRef :: PullRequest -> Text
pullRequestRestyledRef = (<> "-restyled") . pullRequestLocalHeadRef

--------------------------------------------------------------------------------
-- Internal functions below this point
--------------------------------------------------------------------------------

pullRequestOwner :: PullRequest -> SimpleOwner
pullRequestOwner = repoOwner . pullRequestRepo

-- |
--
-- N.B. Partial, we assume a Repo always exists
--
pullRequestRepo :: HasCallStack => PullRequest -> Repo
pullRequestRepo =
    fromJustNote "Pull Request without Repository" . pullRequestBaseRepo

pullRequestBaseRepo :: PullRequest -> Maybe Repo
pullRequestBaseRepo = pullRequestCommitRepo . pullRequestBase

pullRequestHeadRepo :: PullRequest -> Maybe Repo
pullRequestHeadRepo = pullRequestCommitRepo . pullRequestHead
