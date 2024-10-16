{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Module      : Restyler.Options.LogSettings
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.LogSettings
  ( LogSettingsOption (..)
  , resolveLogSettings
  , envLogSettingsOption
  , optLogSettingsOption
  )
where

import Restyler.Prelude

import Blammo.Logging.LogSettings
import Blammo.Logging.LogSettings.Env qualified as LogSettingsEnv
import Blammo.Logging.LogSettings.LogLevels
import Data.Aeson
import Env qualified
import Options.Applicative

instance FromJSON LogColor where
  parseJSON = withText "LogColor" $ either fail pure . readLogColor . unpack

newtype LogSettingsOption = LogSettingsOption
  { unwrap :: Dual (Endo LogSettings)
  }
  deriving newtype (Semigroup, Monoid)

instance FromJSON LogSettingsOption where
  parseJSON = withObject "LogSettings" $ \o ->
    mconcat
      <$> sequenceA
        [ maybe mempty (bool mempty $ setLogLevel LevelDebug) <$> o .:? "debug"
        , maybe mempty (bool mempty $ setLogLevel $ LevelOther "trace") <$> o .:? "trace"
        , maybe mempty setLogColor <$> o .:? "color"
        ]

toLogSettingsOption :: LogSettings -> LogSettingsOption
toLogSettingsOption = LogSettingsOption . Dual . Endo . const

resolveLogSettings :: LogSettingsOption -> LogSettings
resolveLogSettings ls = appEndo (getDual ls.unwrap) defaultLogSettings

envLogSettingsOption :: Env.Parser Env.Error LogSettingsOption
envLogSettingsOption = toLogSettingsOption <$> LogSettingsEnv.parser

optLogSettingsOption :: Parser LogSettingsOption
optLogSettingsOption = mconcat <$> sequenceA [optDebug, optTrace, optColor]

optDebug :: Parser LogSettingsOption
optDebug = optLogLevel "debug" LevelDebug

optTrace :: Parser LogSettingsOption
optTrace = optLogLevel "trace" $ LevelOther "trace"

optLogLevel :: String -> LogLevel -> Parser LogSettingsOption
optLogLevel name level =
  flag mempty (setLogLevel level)
    $ long name
    <> help ("Enable " <> name <> " logging")

optColor :: Parser LogSettingsOption
optColor =
  maybe mempty setLogColor
    <$> optional
      ( option (eitherReader readLogColor)
          $ long "color"
          <> metavar "WHEN"
          <> help "When to use color: always|never|auto"
      )

setLogLevel :: LogLevel -> LogSettingsOption
setLogLevel =
  LogSettingsOption
    . Dual
    . Endo
    . setLogSettingsLevels
    . flip newLogLevels []

setLogColor :: LogColor -> LogSettingsOption
setLogColor =
  LogSettingsOption
    . Dual
    . Endo
    . setLogSettingsColor
