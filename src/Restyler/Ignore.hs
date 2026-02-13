-- |
--
-- Module      : Restyler.Ignore
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Ignore
  ( IgnoredReason (..)
  , getIgnoredReason
  ) where

import Restyler.Prelude

import Data.Aeson (ToJSON)
import Restyler.Config
import Restyler.Config.Glob

data IgnoredReason
  = IgnoredByAuthor Text
  | IgnoredByBranch Text
  | IgnoredByLabels Text
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

getIgnoredReason
  :: Foldable t
  => Ignores
  -> Text
  -- ^ User
  -> Text
  -- ^ Branch
  -> t Text
  -- ^ Labels
  -> Maybe IgnoredReason
getIgnoredReason i author branch labels =
  asum
    [ IgnoredByAuthor author <$ guard (i.byAuthor `matchAny` [author])
    , IgnoredByBranch branch <$ guard (i.byBranch `matchAny` [branch])
    , IgnoredByLabels <$> i.byLabels `globFirst` labels
    ]
