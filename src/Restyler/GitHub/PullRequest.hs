{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GitHub.PullRequest
  ( PullRequest (..)
  , PullRequestState (..)
  , pullRequestStateFromText
  , pullRequestStateToText
  , User (..)
  , Label (..)
  , Commit (..)

    -- * Classy access
  , HasPullRequestState (..)
  , HasAuthor (..)
  , HasBaseRef (..)
  , HasLabelNames (..)
  ) where

import Restyler.Prelude

import Data.Aeson (ToJSON (..))
import Restyler.GitHub.Repository

data PullRequest = PullRequest
  { number :: Int
  , title :: Text
  , user :: User
  , state :: PullRequestState
  , labels :: [Label]
  , head :: Commit
  , base :: Commit
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data PullRequestState
  = PullRequestOpen
  | PullRequestClosed

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

newtype User = User
  { login :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype Label = Label
  { name :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Commit = Commit
  { ref :: Text
  , sha :: Text
  , repo :: Repository
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

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
