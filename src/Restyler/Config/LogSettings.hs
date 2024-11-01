{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Module      : Restyler.Config.LogSettings
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.LogSettings
  ( LogSettings
  , LogSettingsOption
  , logSettingsOptionParser
  , getLogSettings
  , modLogSettings
  )
where

import Restyler.Prelude

import Autodocodec
import Blammo.Logging.LogSettings
import Blammo.Logging.LogSettings.Env qualified as LogSettings
import Blammo.Logging.LogSettings.LogLevels (newLogLevels)
import OptEnvConf

instance HasCodec LogColor where
  codec =
    stringConstCodec
      $ (LogColorAuto, "auto")
      :| [ (LogColorAlways, "always")
         , (LogColorNever, "never")
         ]

newtype Mod a = Mod
  { appMod :: a -> a
  }
  deriving (Semigroup, Monoid) via (Dual (Endo a))

newtype LogSettingsOption = LogSettingsOption
  { unwrap :: Mod LogSettings
  }
  deriving newtype (Semigroup, Monoid)

getLogSettings :: LogSettingsOption -> IO LogSettings
getLogSettings settings = modLogSettings settings <$> LogSettings.parse

modLogSettings :: LogSettingsOption -> LogSettings -> LogSettings
modLogSettings = (.unwrap.appMod)

logSettingsOptionParser :: Parser LogSettingsOption
logSettingsOptionParser =
  mconcat
    <$> sequenceA
      [ levelsParser <|> levelParser "debug" LevelDebug
      , levelsParser <|> levelParser "trace" (LevelOther "trace")
      , colorParser
      ]

-- | We'll try to read LOG_LEVELS first, so its respected
levelsParser :: Parser LogSettingsOption
levelsParser =
  LogSettingsOption
    . Mod
    . setLogSettingsLevels
    <$> setting
      [ help ""
      , reader $ eitherReader readLogLevels
      , env "LOG_LEVEL"
      , hidden
      ]

levelParser :: String -> LogLevel -> Parser LogSettingsOption
levelParser levelName level =
  LogSettingsOption
    . Mod
    . bool id (setLogLevel level)
    <$> withDefault
      False
      ( yesNoSwitch
          [ help $ "Enable " <> levelName <> " logging"
          , name levelName
          ]
      )

colorParser :: Parser LogSettingsOption
colorParser =
  LogSettingsOption
    . Mod
    . setLogSettingsColor
    <$> setting
      [ help "Enable color WHEN"
      , option
      , name "color"
      , metavar "WHEN"
      , reader $ eitherReader readLogColor
      , env "LOG_COLOR"
      , value LogColorAuto
      ]

setLogLevel :: LogLevel -> LogSettings -> LogSettings
setLogLevel = setLogSettingsLevels . flip newLogLevels []
