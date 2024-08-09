-- |
--
-- Module      : Restyler.Config.CommitTemplate
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.CommitTemplate
  ( CommitTemplate (..)
  , CommitTemplateInputs (..)
  , renderCommitTemplate
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.Restyler

newtype CommitTemplateInputs = CommitTemplateInputs
  { restyler :: Restyler
  }

newtype CommitTemplate = CommitTemplate
  { unwrap :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

renderCommitTemplate :: CommitTemplateInputs -> CommitTemplate -> String
renderCommitTemplate cti =
  unpack
    . replaceAll [("${restyler.name}", pack $ rName cti.restyler)]
    . (.unwrap)

-- | Let's make this as unreadable as possible, shall we?
replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll = flip $ foldl' $ flip $ uncurry T.replace
