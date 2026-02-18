-- |
--
-- Module      : Restyler.Config.RemoteFile
-- Copyright   : (c) 2025 Patrick Brisbin
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
import Path (parseRelFile)

class HasRemoteFiles env where
  getRemoteFiles :: env -> [RemoteFile]

data RemoteFile = RemoteFile
  { url :: String
  , path :: Path Rel File
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

remoteFileFromPair
  :: (String, Maybe (Path Rel File)) -> Either String RemoteFile
remoteFileFromPair (url, mPath) = case mPath of
  Nothing -> remoteFileFromUrl url
  Just path -> Right $ RemoteFile {url, path}

remoteFileToPair :: RemoteFile -> (String, Maybe (Path Rel File))
remoteFileToPair rf = (rf.url, Just rf.path)

codecUrl :: JSONCodec RemoteFile
codecUrl = bimapCodec remoteFileFromUrl remoteFileToUrl stringCodec <?> "URL with path"

remoteFileFromUrl :: String -> Either String RemoteFile
remoteFileFromUrl url = do
  uri <- note "" $ parseAbsoluteURI url
  segs <-
    note "RemoteFile's URL has no path, and one is not configured"
      $ nonEmpty
      $ URI.pathSegments uri
  path <-
    note "RemoteFile's URL path is not a valid file name" $ parseRelFile $ last segs
  pure $ RemoteFile {url, path}

-- | Invalid, but not the codec is never used to render
remoteFileToUrl :: RemoteFile -> String
remoteFileToUrl = (.url)

remoteFilesParser :: Parser [RemoteFile]
remoteFilesParser =
  setting
    [ help
        $ unpack
        $ unlines
          [ "Download remote file before restyling"
          ]
    , example
        $ unpack
        $ unlines
          [ "remote_files:"
          , "  # URL downloaded to a particular path"
          , "  - url: https://raw.github.com/.../hlint.yaml"
          , "    path: .hlint.yaml"
          ]
    , example
        $ unpack
        $ unlines
          [ "remote_files:"
          , "  # URL downloaded to its basename"
          , "  - https://raw.github.com/.../prettier.json"
          ]
    , conf "remote_files"
    , value []
    ]
