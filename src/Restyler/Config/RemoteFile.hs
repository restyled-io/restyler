module Restyler.Config.RemoteFile
  ( RemoteFile (..)
  ) where

import Restyler.Prelude

data RemoteFile = RemoteFile
  { url :: URL
  , path :: FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
