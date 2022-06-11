module Restyler.CommitTemplate
    ( CommitTemplate
    , commitTemplate
    , CommitTemplateInputs(..)
    , renderCommitTemplate
    ) where

import Restyler.Prelude

import Data.Aeson
import qualified Data.Text as T
import Restyler.Restyler

newtype CommitTemplateInputs = CommitTemplateInputs
    { ctiRestyler :: Restyler
    }

newtype CommitTemplate = CommitTemplate
    { unCommitTemplate :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

commitTemplate :: Text -> CommitTemplate
commitTemplate = CommitTemplate

renderCommitTemplate :: CommitTemplateInputs -> CommitTemplate -> String
renderCommitTemplate CommitTemplateInputs {..} =
    unpack
        . replaceAll [("${restyler.name}", pack $ rName ctiRestyler)]
        . unCommitTemplate

-- | Let's make this as unreadable as possible, shall we?
replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll = flip $ foldl' $ flip $ uncurry T.replace
