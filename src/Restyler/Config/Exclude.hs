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
      [ help "Globs to exclude"
      , option
      , long "exclude"
      , reader $ commaSeparatedList str
      , metavar "GLOB[,GLOB...]"
      , conf "exclude"
      ]
    <*> setting
      [ help "Globs to exclude in addition to defaults"
      , option
      , long "also-exclude"
      , reader $ commaSeparatedList str
      , metavar "GLOB[,GLOB...]"
      , conf "also_exclude"
      ]
