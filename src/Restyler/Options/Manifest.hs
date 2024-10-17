-- |
--
-- Module      : Restyler.Options.Manifest
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.Manifest
  ( HasManifest (..)
  , manifestParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasManifest env where
  getManifest :: env -> Maybe (Path Abs File)

manifestParser :: Parser (Path Abs File)
manifestParser =
  filePathSetting
    [ help "Restylers manifest to use"
    , option
    , long "manifest"
    , env "MANIFEST"
    , conf "manifest"
    ]
