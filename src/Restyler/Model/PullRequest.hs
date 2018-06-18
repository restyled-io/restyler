{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Model.PullRequest
    ( pullRequestOwnerName
    , pullRequestRepoName
    , pullRequestCloneUrl
    , pullRequestSpec
    , pullRequestIssueId
    , pullRequestIsFork
    , pullRequestBaseRef
    , pullRequestHeadRef
    , pullRequestRemoteHeadRef
    , pullRequestLocalHeadRef
    , pullRequestRestyledBase
    , pullRequestRestyledRef
    )
where

import Restyler.Prelude

import Restyler.Model.PullRequestSpec

pullRequestOwnerName :: PullRequest -> Name Owner
pullRequestOwnerName = simpleOwnerLogin . pullRequestOwner

pullRequestRepoName :: HasCallStack => PullRequest -> Name Repo
pullRequestRepoName = repoName . pullRequestRepo

pullRequestCloneUrl :: HasCallStack => PullRequest -> URL
pullRequestCloneUrl pullRequest = fromMaybe (repoUrl repo) $ repoCloneUrl repo
    where repo = pullRequestRepo pullRequest

pullRequestSpec :: HasCallStack => PullRequest -> PullRequestSpec
pullRequestSpec pullRequest = PullRequestSpec
    { prsOwner = pullRequestOwnerName pullRequest
    , prsRepo = pullRequestRepoName pullRequest
    , prsPullRequest = pullRequestNumber pullRequest
    }

-- | Some API actions need to treat the PR like an Issue
pullRequestIssueId :: PullRequest -> Id Issue
pullRequestIssueId = mkId Proxy . pullRequestNumber

-- brittany-disable-next-binding
pullRequestIsFork :: PullRequest -> Bool
pullRequestIsFork = (/=) <$> pullRequestHeadRepo <*> pullRequestBaseRepo

pullRequestBaseRef :: PullRequest -> Text
pullRequestBaseRef = pullRequestCommitRef . pullRequestBase

pullRequestHeadRef :: PullRequest -> Text
pullRequestHeadRef = pullRequestCommitRef . pullRequestHead

-- brittany-disable-next-binding
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
