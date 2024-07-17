module Restyler.LogSettingsOption
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
optLogSettingsOption = mconcat <$> sequenceA [optDebug, optColor]

optDebug :: Parser LogSettingsOption
optDebug = flag mempty setDebug $ long "debug" <> help "Enable debug logging"
 where
  setDebug = LogSettingsOption $ Dual . Endo $ setLogSettingsLevels debug
  debug = newLogLevels LevelDebug []

optColor :: Parser LogSettingsOption
optColor =
  maybe mempty (LogSettingsOption . Dual . Endo . setLogSettingsColor)
    <$> optional
      ( option (eitherReader readLogColor)
          $ long "color"
          <> metavar "WHEN"
          <> help "When to use color: always|never|auto"
      )
