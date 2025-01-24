-- |
--
-- Module      : Restyler.Config.CommitTemplate
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.CommitTemplate
  ( HasCommitTemplate (..)
  , CommitTemplate (..)
  , commitTemplateParser
  , CommitTemplateInputs (..)
  , renderCommitTemplate
  ) where

import Restyler.Prelude

import Autodocodec (HasCodec)
import Data.Text qualified as T
import OptEnvConf

class HasCommitTemplate env where
  getCommitTemplate :: env -> CommitTemplate

newtype CommitTemplate = CommitTemplate
  { unwrap :: Text
  }
  deriving stock (Eq)
  deriving newtype (Show, IsString, HasCodec)

commitTemplateParser :: Parser CommitTemplate
commitTemplateParser =
  setting
    [ help "Template for restyling commit messages"
    , conf "commit_template"
    , value "Restyled by ${restyler.name}\n"
    ]

newtype CommitTemplateInputs = CommitTemplateInputs
  { restyler :: Text
  }

renderCommitTemplate :: CommitTemplateInputs -> CommitTemplate -> String
renderCommitTemplate cti =
  unpack
    . replaceAll [("${restyler.name}", cti.restyler)]
    . (.unwrap)

-- | Let's make this as unreadable as possible, shall we?
replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll = flip $ foldl' $ flip $ uncurry T.replace
