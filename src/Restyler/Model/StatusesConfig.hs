{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Model.StatusesConfig
    ( StatusesConfig(..)
    , defaultStatusesConfig
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson.Types as Aeson

-- | Configuration for sending PR statuses
data StatusesConfig = StatusesConfig
    { scDifferences :: Bool
    -- ^ Send a failure status when there were differences
    , scNoDifferences :: Bool
    -- ^ Send a success status when there were no differences
    , scError :: Bool
    -- ^ Send a failure status when there were errors
    }
    deriving (Eq, Show, Generic)

instance FromJSON StatusesConfig where
    parseJSON (Object o) = StatusesConfig
        <$> o .:? "differences" .!= scDifferences
        <*> o .:? "no-differences" .!= scNoDifferences
        <*> o .:? "error" .!= scError
      where
        StatusesConfig{..} = defaultStatusesConfig
    parseJSON (Aeson.Bool b) = pure StatusesConfig
        { scDifferences = b
        , scNoDifferences = b
        , scError = b
        }
    parseJSON x = typeMismatch "Boolean or Statuses Configuration" x

instance ToJSON StatusesConfig where
    -- N.B. this is incorrect because I stupidly made this one kebab-case, but
    -- we only use this instance for DEBUG printing, so it's fine for now.
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

defaultStatusesConfig :: StatusesConfig
defaultStatusesConfig = StatusesConfig
    { scDifferences = True
    , scNoDifferences = True
    , scError = True
    }
