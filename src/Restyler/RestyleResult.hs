module Restyler.RestyleResult
  ( RestyleResult (..)
  , RestyleSkipped (..)
  ) where

import Restyler.Prelude

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
