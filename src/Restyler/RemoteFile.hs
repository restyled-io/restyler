module Restyler.RemoteFile
    ( RemoteFile(..)
    , downloadRemoteFile
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Restyler.Capabilities.DownloadFile
import Restyler.Config.ExpectedKeys

data RemoteFile = RemoteFile
    { rfUrl :: URL
    , rfPath :: FilePath
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON RemoteFile where
    parseJSON = genericParseJSONValidated $ aesonPrefix snakeCase

instance ToJSON RemoteFile where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

downloadRemoteFile :: MonadDownloadFile m => RemoteFile -> m ()
downloadRemoteFile RemoteFile {..} = downloadFile (getUrl rfUrl) rfPath
