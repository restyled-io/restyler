-- |
--
-- Module      : Restyler.Ignore
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Ignore
  ( IgnoredReason (..)
  , getIgnoredReason
  ) where

import Restyler.Prelude

import Restyler.Config
import Restyler.Config.Glob

data IgnoredReason
  = IgnoredByAuthor Text
  | IgnoredByBranch Text
  | IgnoredByLabels Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

getIgnoredReason
  :: Foldable t
  => Config
  -> Text
  -- ^ User
  -> Text
  -- ^ Branch
  -> t Text
  -- ^ Labels
  -> Maybe IgnoredReason
getIgnoredReason c author branch labels =
  asum
    [ IgnoredByAuthor author <$ guard (cIgnoreAuthors c `matchAny` [author])
    , IgnoredByBranch branch <$ guard (cIgnoreBranches c `matchAny` [branch])
    , IgnoredByLabels <$> cIgnoreLabels c `matchFirst` labels
    ]
