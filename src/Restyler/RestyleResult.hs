module Restyler.RestyleResult
  ( RestyleResult (..)
  , RestyleSkipped (..)
  ) where

import Restyler.Prelude

import Restyler.Ignore

data RestyleResult
  = RestyleSkipped RestyleSkipped
  | RestyleSuccessNoDifference
  | RestyleSuccessDifference

data RestyleSkipped
  = RestyleNotEnabled
  | RestylePullRequestClosed
  | RestyleIgnored IgnoredReason
  deriving stock (Generic)
  deriving anyclass (ToJSON)
