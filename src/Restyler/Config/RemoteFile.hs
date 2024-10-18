-- |
--
-- Module      : Restyler.Config.RemoteFile
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.RemoteFile
  ( HasRemoteFiles (..)
  , RemoteFile (..)
  , remoteFilesParser
  ) where

import Restyler.Prelude hiding ((.=))

import Autodocodec
import OptEnvConf

class HasRemoteFiles env where
  getRemoteFiles :: env -> [RemoteFile]

data RemoteFile = RemoteFile
  { url :: String
  , path :: FilePath
  }
  deriving stock (Eq, Show)

instance HasCodec RemoteFile where
  codec =
    object "RemoteFile"
      $ RemoteFile
      <$> (requiredField "url" "URL to download" .= (.url))
      <*> (requiredField "path" "Path to download to" .= (.path))

remoteFilesParser :: Parser [RemoteFile]
remoteFilesParser =
  setting
    [ help "Download remote file before restyling"
    , conf "remote_files"
    ]
