{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Restyler.PullRequest
    ( pullRequestOwner
    , pullRequestRepo
    , pullRequestRepoURL
    , pullRequestIsFork
    ) where

import ClassyPrelude

import GitHub.Data

pullRequestOwner :: PullRequest -> SimpleOwner
pullRequestOwner = repoOwner . pullRequestRepo

pullRequestRepo :: PullRequest -> Repo
pullRequestRepo = pullRequestCommitRepo . pullRequestBase

pullRequestRepoURL :: PullRequest -> Text
pullRequestRepoURL pullRequest =
    "https://github.com/" <> owner <> "/" <> repo
  where
    owner = untagName $ simpleOwnerLogin $ pullRequestOwner pullRequest
    repo = untagName $ repoName $ pullRequestRepo pullRequest

pullRequestIsFork :: PullRequest -> Bool
pullRequestIsFork pullRequest =
    pullRequestCommitRepo (pullRequestHead pullRequest) /=
    pullRequestCommitRepo (pullRequestBase pullRequest)
