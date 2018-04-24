{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.PullRequest
    ( pullRequestOwner
    , pullRequestRepo
    , pullRequestRepoURL
    , pullRequestIsFork
    , pullRequestBaseRef
    , pullRequestHeadRef
    , pullRequestLocalHeadRef
    , pullRequestRestyledRef
    , restyledCreatePullRequest
    ) where

import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.Stack
import GitHub.Data

pullRequestOwner :: PullRequest -> SimpleOwner
pullRequestOwner = repoOwner . pullRequestRepo

pullRequestRepo :: HasCallStack => PullRequest -> Repo
pullRequestRepo =
    -- We always expect a repo on our PRs
    fromMaybe (error "Pull Request without Repository")
        . pullRequestCommitRepo
        . pullRequestBase

pullRequestRepoURL :: PullRequest -> Text
pullRequestRepoURL pullRequest = "https://github.com/" <> owner <> "/" <> repo
  where
    owner = untagName $ simpleOwnerLogin $ pullRequestOwner pullRequest
    repo = untagName $ repoName $ pullRequestRepo pullRequest

pullRequestIsFork :: PullRequest -> Bool
pullRequestIsFork pullRequest =
    pullRequestCommitRepo (pullRequestHead pullRequest)
        /= pullRequestCommitRepo (pullRequestBase pullRequest)

pullRequestBaseRef :: PullRequest -> Text
pullRequestBaseRef PullRequest{..} =
    pullRequestCommitRef pullRequestBase

pullRequestHeadRef :: PullRequest -> Text
pullRequestHeadRef PullRequest{..} =
    pullRequestCommitRef pullRequestHead

pullRequestLocalHeadRef :: PullRequest -> Text
pullRequestLocalHeadRef pullRequest@PullRequest{..}
    | pullRequestIsFork pullRequest = "pull-" <> toPathPart pullRequestId
    | otherwise = pullRequestCommitRef pullRequestHead

pullRequestRestyledRef :: PullRequest -> Text
pullRequestRestyledRef = (<> "-restyled") . pullRequestLocalHeadRef

restyledCreatePullRequest :: PullRequest -> CreatePullRequest
restyledCreatePullRequest pullRequest@PullRequest{..} = CreatePullRequest
    { createPullRequestTitle = pullRequestTitle <> " (Restyled)"
    , createPullRequestBody = ""
    , createPullRequestHead = pullRequestRestyledRef pullRequest
    , createPullRequestBase =
        -- We can't open a PR in their fork, so we open a PR in our own
        -- repository against the base branch. It'll have their changes
        -- and our restyling as separate commits.
        if pullRequestIsFork pullRequest
            then pullRequestBaseRef pullRequest
            else pullRequestHeadRef pullRequest
    }
