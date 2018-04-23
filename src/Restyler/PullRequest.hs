{-# LANGUAGE OverloadedStrings #-}
module Restyler.PullRequest
    ( pullRequestOwner
    , pullRequestRepo
    , pullRequestRepoURL
    , pullRequestIsFork
    ) where

import GitHub.Data
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Semigroup ((<>))
import GHC.Stack

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
