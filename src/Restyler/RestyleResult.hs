-- |
--
-- Module      : Restyler.RestyleResult
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.RestyleResult
  ( RestyleResult (..)
  , RestyleSkipped (..)
  ) where

import Restyler.Prelude

import Restyler.Ignore

data RestyleResult
  = RestyleSkipped RestyleSkipped
  | RestyleNoDifference
  | RestyleDifference

data RestyleSkipped
  = RestyleNotEnabled
  | RestylePullRequestClosed
  | RestyleIgnored IgnoredReason
  deriving stock (Generic)
  deriving anyclass (ToJSON)
