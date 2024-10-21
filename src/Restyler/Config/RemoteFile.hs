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
import Network.URI (parseAbsoluteURI)
import Network.URI qualified as URI
import OptEnvConf

class HasRemoteFiles env where
  getRemoteFiles :: env -> [RemoteFile]

data RemoteFile = RemoteFile
  { url :: String
  , path :: FilePath
  }
  deriving stock (Eq, Show)

instance HasCodec RemoteFile where
  codec = parseAlternatives codecObject [codecUrl]

codecObject :: JSONCodec RemoteFile
codecObject =
  bimapCodec remoteFileFromPair remoteFileToPair
    $ object "RemoteFile"
    $ (,)
    <$> (requiredField "url" "URL to download" .= fst)
    <*> (optionalField "path" "Path to download to" .= snd)

remoteFileFromPair :: (String, Maybe String) -> Either String RemoteFile
remoteFileFromPair (url, mPath) = case mPath of
  Nothing -> remoteFileFromUrl url
  Just path -> Right $ RemoteFile {url, path}

remoteFileToPair :: RemoteFile -> (String, Maybe String)
remoteFileToPair rf = (rf.url, Just rf.path)

codecUrl :: JSONCodec RemoteFile
codecUrl = bimapCodec remoteFileFromUrl remoteFileToUrl stringCodec <?> "URL with path"

remoteFileFromUrl :: String -> Either String RemoteFile
remoteFileFromUrl url = do
  uri <- note "" $ parseAbsoluteURI url
  case nonEmpty $ URI.pathSegments uri of
    Nothing -> Left "RemoteFile's URL has no path, and one is not configured"
    Just ps -> Right $ RemoteFile {url, path = last ps}

-- | Invalid, but not the codec is never used to render
remoteFileToUrl :: RemoteFile -> String
remoteFileToUrl = (.url)

remoteFilesParser :: Parser [RemoteFile]
remoteFilesParser =
  setting
    [ help "Download remote file before restyling"
    , conf "remote_files"
    ]
