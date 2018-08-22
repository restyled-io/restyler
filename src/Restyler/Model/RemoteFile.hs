{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Restyler.Model.RemoteFile
    ( RemoteFile(..)
    ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Restyler.Model.Config.ExpectedKeys

-- | A remote (configuration) file, to fetch before restyling
data RemoteFile = RemoteFile
    { rfUrl :: URL -- ^ Re-using GitHub's URL type
    , rfPath :: FilePath
    }
    deriving (Eq, Show, Generic)

instance FromJSON RemoteFile where
    parseJSON = withObject "RemoteFile" $ \o -> do
        validateObjectKeys ["url", "path"] o
        RemoteFile
            <$> o .: "url"
            <*> o .: "path"

instance ToJSON RemoteFile where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase
