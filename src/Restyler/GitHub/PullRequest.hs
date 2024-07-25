module Restyler.GitHub.PullRequest
  ( PullRequest (..)
  , PullRequestState (..)
  , User (..)
  , Label (..)
  , Commit (..)
  , Repo (..)
  , Owner (..)

    -- * Classy access
  , HasHtmlUrl (..)
  , HasNumber (..)
  , HasPullRequestState (..)
  , HasAuthor (..)
  , HasBaseRef (..)
  , HasHeadSha (..)
  , HasLabelNames (..)
  ) where

import Restyler.Prelude

import Data.Aeson (FromJSON (..), ToJSON (..), withText)

data PullRequest = PullRequest
  { html_url :: URL
  , number :: Int
  , title :: Text
  , user :: User
  , state :: PullRequestState
  , labels :: [Label]
  , head :: Commit
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

class HasHtmlUrl a where
  getHtmlUrl :: a -> URL

instance HasHtmlUrl PullRequest where
  getHtmlUrl pr = pr.html_url

class HasNumber a where
  getNumber :: a -> Int

instance HasNumber PullRequest where
  getNumber pr = pr.number

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

class HasHeadSha a where
  getHeadSha :: a -> Text

instance HasHeadSha PullRequest where
  getHeadSha pr = pr.head.sha

class HasLabelNames a where
  getLabelNames :: a -> [Text]

instance HasLabelNames PullRequest where
  getLabelNames pr = map (.name) pr.labels
