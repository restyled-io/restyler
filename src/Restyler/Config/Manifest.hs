-- |
--
-- Module      : Restyler.Config.Manifest
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Manifest
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
    , long "manifest" -- backwards-compatible
    , env "MANIFEST" -- backwards-compatible
    , conf "restylers_manifest"
    ]
