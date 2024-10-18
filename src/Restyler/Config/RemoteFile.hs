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

instance HasCodec RemoteFile where
  codec =
    object "RemoteFile"
      $ RemoteFile
      <$> (requiredField "url" "URL to download" .= (.url))
      <*> (requiredField "path" "Path to download to" .= (.path))

-- |
--
-- TODO: I swear we used to parse a simple String as a URL and then download it
-- to the base-name of that URL. Where did that go?
remoteFilesParser :: Parser [RemoteFile]
remoteFilesParser =
  setting
    [ help "Download remote file before restyling"
    , conf "remote_files"
    ]
