-- |
--
-- Module      : Restyler.Config.Exclude
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Exclude
  ( HasExclude (..)
  , excludeParser
  ) where

import Restyler.Prelude

import OptEnvConf
import Restyler.Config.Glob

class HasExclude env where
  getExclude :: env -> [Glob FilePath]

excludeParser :: Parser [Glob FilePath]
excludeParser =
  (<>)
    <$> setting
      [ help "Exclude paths matching the given globs (instead of defaults)"
      , option
      , name "exclude"
      , reader $ commaSeparatedList str
      , metavar "GLOB[,GLOB]"
      , value
          [ "**/*.patch"
          , "**/.git/**/*"
          , "**/node_modules/**/*"
          , "**/vendor/**/*"
          , ".github/workflows/**/*"
          ]
      ]
    <*> setting
      [ help "Exclude paths matching the given globs (in addition to defaults)"
      , option
      , long "also-exclude"
      , env "ALSO_EXCLUDE"
      , reader $ commaSeparatedList str
      , metavar "GLOB[,GLOB]"
      , conf "also_exclude"
      , value []
      ]
