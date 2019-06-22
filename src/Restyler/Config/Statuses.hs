module Restyler.Config.Statuses
    ( Statuses(..)
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson.Types as Aeson
import Restyler.Config.ExpectedKeys

-- | Configuration for sending PR statuses
data Statuses = Statuses
    { sDifferences :: Bool
    -- ^ Send a failure status when there were differences
    , sNoDifferences :: Bool
    -- ^ Send a success status when there were no differences
    , sError :: Bool
    -- ^ Send a failure status when there were errors
    }
    deriving (Eq, Show, Generic)

instance FromJSON Statuses where
    parseJSON (Object o) = do
        validateObjectKeys
            ["differences", "no_differences", "no-differences", "error"] o

        let noDifferences = (<|>)
                -- N.B. Snake-case preferred, kebab-case is deprecated
                <$> o .:? "no_differences"
                <*> o .:? "no-differences"

        Statuses
            <$> o .:? "differences" .!= True
            <*> noDifferences .!= True
            <*> o .:? "error" .!= True
    parseJSON (Aeson.Bool b) = pure Statuses
        { sDifferences = b
        , sNoDifferences = b
        , sError = b
        }
    parseJSON x = typeMismatch "Boolean or Statuses Configuration" x

instance ToJSON Statuses where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase
