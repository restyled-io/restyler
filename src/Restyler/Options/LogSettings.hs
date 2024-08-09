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
import Env qualified
import Options.Applicative

newtype LogSettingsOption = LogSettingsOption (Dual (Endo LogSettings))
  deriving newtype (Semigroup, Monoid)

toLogSettingsOption :: LogSettings -> LogSettingsOption
toLogSettingsOption = LogSettingsOption . Dual . Endo . const

resolveLogSettings :: LogSettingsOption -> LogSettings
resolveLogSettings (LogSettingsOption f) = appEndo (getDual f) defaultLogSettings

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
  flag mempty setLogLevels
    $ long name
    <> help ("Enable " <> name <> " logging")
 where
  setLogLevels =
    LogSettingsOption
      $ Dual
      . Endo
      $ setLogSettingsLevels
      $ newLogLevels level []

optColor :: Parser LogSettingsOption
optColor =
  maybe mempty (LogSettingsOption . Dual . Endo . setLogSettingsColor)
    <$> optional
      ( option (eitherReader readLogColor)
          $ long "color"
          <> metavar "WHEN"
          <> help "When to use color: always|never|auto"
      )
