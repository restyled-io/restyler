module Restyler.RestyleResult
  ( RestyleResult (..)
  , RestyleSkipped (..)
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import GitHub qualified
import Restyler.Config
import Restyler.Config.RequestReview
import Restyler.Content (pullRequestDescription)
import Restyler.GHA.Output
import Restyler.Git
import Restyler.GitHub.PullRequest
import Restyler.Ignore
import Restyler.RestylerResult

data RestyleResult
  = RestyleSkipped RestyleSkipped
  | RestyleSuccessNoDifference
  | RestyleSuccessDifference [RestylerResult]

data RestyleSkipped
  = RestyleNotEnabled
  | RestylePullRequestClosed
  | RestyleIgnored IgnoredReason
  deriving stock (Generic)
  deriving anyclass (ToJSON)
