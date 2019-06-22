module Restyler.RemoteFile
    ( RemoteFile(..)
    , downloadRemoteFile
    ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Restyler.App.Class
import Restyler.Config.ExpectedKeys

data RemoteFile = RemoteFile
    { rfUrl :: URL
    , rfPath :: FilePath
    }
    deriving (Eq, Show, Generic)

instance FromJSON RemoteFile where
    parseJSON = genericParseJSONValidated $ aesonPrefix snakeCase

instance ToJSON RemoteFile where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

downloadRemoteFile
    :: (HasLogFunc env, HasDownloadFile env) => RemoteFile -> RIO env ()
downloadRemoteFile RemoteFile {..} = do
    logInfo $ fromString $ "Fetching remote file: " <> rfPath
    downloadFile (getUrl rfUrl) rfPath
