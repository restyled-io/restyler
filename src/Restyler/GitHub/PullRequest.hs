-- |
--
-- Module      : Restyler.GitHub.PullRequest
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.GitHub.PullRequest
  ( PullRequest
  , HasPullRequestState (..)
  , PullRequestState (..)
  , HasAuthor (..)
  , HasBaseRef (..)
  , HasLabelNames (..)

    -- * Faking the PR interface
  , NullPullRequest (..)
  ) where

import Restyler.Prelude

import Data.Aeson (FromJSON (..), ToJSON (..), withText)

data PullRequest = PullRequest
  { title :: Text
  , user :: User
  , state :: PullRequestState
  , labels :: [Label]
  , base :: Commit
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PullRequestState
  = PullRequestOpen
  | PullRequestClosed
  deriving stock (Show)

instance FromJSON PullRequestState where
  parseJSON =
    withText "PullRequestState"
      $ either fail pure
      . pullRequestStateFromText

instance ToJSON PullRequestState where
  toJSON = toJSON . pullRequestStateToText
  toEncoding = toEncoding . pullRequestStateToText

pullRequestStateToText :: PullRequestState -> Text
pullRequestStateToText = \case
  PullRequestOpen -> "open"
  PullRequestClosed -> "closed"

pullRequestStateFromText :: Text -> Either String PullRequestState
pullRequestStateFromText = \case
  "open" -> Right PullRequestOpen
  "closed" -> Right PullRequestClosed
  x -> Left $ "Unexpected PullRequestState: " <> show x

newtype User = User
  { login :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Label = Label
  { name :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Commit = Commit
  { ref :: Text
  , sha :: Text
  , repo :: Repo
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Repo = Repo
  { name :: Text
  , owner :: Owner
  , private :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Owner = Owner
  { login :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

class HasPullRequestState a where
  getPullRequestState :: a -> PullRequestState

instance HasPullRequestState PullRequest where
  getPullRequestState pr = pr.state

class HasAuthor a where
  getAuthor :: a -> Text

instance HasAuthor PullRequest where
  getAuthor pr = pr.user.login

class HasBaseRef a where
  getBaseRef :: a -> Text

instance HasBaseRef PullRequest where
  getBaseRef pr = pr.base.ref

class HasLabelNames a where
  getLabelNames :: a -> [Text]

instance HasLabelNames PullRequest where
  getLabelNames pr = map (.name) pr.labels

-- | A 'PullRequest'-like object designed to never match the state or ignore
-- checks we do here when running against a real PR.
data NullPullRequest = NullPullRequest

instance HasPullRequestState NullPullRequest where
  getPullRequestState = const PullRequestOpen

instance HasAuthor NullPullRequest where
  getAuthor = const "NONE"

instance HasBaseRef NullPullRequest where
  getBaseRef = const "UNKNOWN"

instance HasLabelNames NullPullRequest where
  getLabelNames = const []
