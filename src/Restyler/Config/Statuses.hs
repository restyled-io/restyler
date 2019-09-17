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

data Statuses = Statuses
    { sDifferences :: Bool
    , sNoDifferences :: Bool
    , sError :: Bool
    }
    deriving (Eq, Show, Generic)

-- brittany-disable-next-binding

instance FromJSON Statuses where
    parseJSON (Object o) = do
        validateObjectKeys ["differences", "no_differences", "error"] o
        Statuses
            <$> o .:? "differences" .!= True
            <*> o .:? "no_differences" .!= True
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
