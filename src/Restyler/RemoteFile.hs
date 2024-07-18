module Restyler.RemoteFile
  ( RemoteFile (..)
  , downloadRemoteFile
  ) where

import Restyler.Prelude

import Data.Aeson
import Restyler.App.Class (MonadDownloadFile (..))

data RemoteFile = RemoteFile
  { url :: URL
  , path :: FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

downloadRemoteFile
  :: (MonadLogger m, MonadDownloadFile m) => RemoteFile -> m ()
downloadRemoteFile rf = do
  logInfo $ "Fetching remote file" :# objectToPairs rf
  downloadFile (getUrl rf.url) rf.path
