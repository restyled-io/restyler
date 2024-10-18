-- |
--
-- Module      : Restyler.Options.HostDirectory
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.HostDirectory
  ( HasHostDirectory (..)
  , hostDirectoryParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasHostDirectory env where
  getHostDirectory :: env -> Path Abs Dir

hostDirectoryParser :: Parser (Path Abs Dir)
hostDirectoryParser =
  directoryPathSetting
    [ help "Working directory on host, if dockerized"
    , option
    , long "host-directory"
    , env "HOST_DIRECTORY"
    , conf "host_directory"
    ]
