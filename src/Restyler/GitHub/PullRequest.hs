{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GitHub.PullRequest
  ( PullRequest (..)
  , PullRequestState (..)
  , pullRequestStateFromText
  , pullRequestStateToText
  , Label (..)
  , Commit (..)
  , getPullRequest
  ) where

import Restyler.Prelude

import Data.Aeson
import Restyler.GitHub.Api
import Restyler.GitHub.Repository

data PullRequest = PullRequest
  { number :: Int
  , state :: PullRequestState
  , labels :: [Label]
  , head :: Commit
  , base :: Commit
  , draft :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data PullRequestState
  = PullRequestOpen
  | PullRequestClosed

instance FromJSON PullRequestState where
  parseJSON = withText "state" $ either fail pure . pullRequestStateFromText

instance ToJSON PullRequestState where
  toJSON = toJSON . pullRequestStateToText
  toEncoding = toEncoding . pullRequestStateToText

pullRequestStateFromText :: Text -> Either String PullRequestState
pullRequestStateFromText = \case
  "open" -> Right PullRequestOpen
  "closed" -> Right PullRequestClosed
  x -> Left $ "Invalid state " <> show x <> ", must be open or closed"

pullRequestStateToText :: PullRequestState -> Text
pullRequestStateToText = \case
  PullRequestOpen -> "open"
  PullRequestClosed -> "closed"

newtype Label = Label
  { name :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data Commit = Commit
  { ref :: Text
  , sha :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

getPullRequest
  :: (MonadIO m, MonadReader env m, HasGitHubToken env)
  => Repository
  -> Int
  -> m PullRequest
getPullRequest repo pr =
  getOne
    $ "https://api.github.com/repos/"
    <> unpack repo.owner
    <> "/"
    <> unpack repo.repo
    <> "/pulls/"
    <> show pr
