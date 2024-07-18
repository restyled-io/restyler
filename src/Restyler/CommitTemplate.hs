module Restyler.CommitTemplate
  ( CommitTemplate
  , commitTemplate
  , CommitTemplateInputs (..)
  , renderCommitTemplate
  ) where

import Restyler.Prelude

import Data.Aeson
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

commitTemplate :: Text -> CommitTemplate
commitTemplate = CommitTemplate

renderCommitTemplate :: CommitTemplateInputs -> CommitTemplate -> String
renderCommitTemplate cti =
  unpack
    . replaceAll [("${restyler.name}", pack $ rName cti.restyler)]
    . (.unwrap)

-- | Let's make this as unreadable as possible, shall we?
replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll = flip $ foldl' $ flip $ uncurry T.replace
