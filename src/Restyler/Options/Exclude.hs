module Restyler.Options.Exclude
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
  mconcat
    <$> sequenceA
      [ setting
          [ help "Globs to exclude"
          , conf "exclude"
          ]
      , setting
          [ help "Globs to exclude in addition to defaults"
          , conf "also_exclude"
          ]
      , many
          ( setting
              [ help "Globs to exclude in addition to defaults"
              , option
              , long "exclude"
              , reader str
              , metavar "GLOB"
              ]
          )
      ]
