-- |
--
-- Module      : Restyler.Config.RemoteFile
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.RemoteFile
  ( RemoteFile (..)
  ) where

import Restyler.Prelude

data RemoteFile = RemoteFile
  { url :: String
  , path :: FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
