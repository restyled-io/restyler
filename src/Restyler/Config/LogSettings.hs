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
import Blammo.Logging.LogSettings.LogLevels
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
      [ levelParser "debug" $ bool id $ setLogLevel LevelDebug
      , levelParser "trace" $ bool id $ setLogLevel $ LevelOther "trace"
      , colorParser setLogSettingsColor
      ]

levelParser
  :: String -> (Bool -> LogSettings -> LogSettings) -> Parser LogSettingsOption
levelParser level f =
  LogSettingsOption
    . Mod
    . f
    <$> yesNoSwitch
      [ help $ "Enable " <> level <> " logging"
      , name level
      ]

colorParser
  :: (LogColor -> LogSettings -> LogSettings) -> Parser LogSettingsOption
colorParser f =
  LogSettingsOption
    . Mod
    . f
    <$> setting
      [ help "Enable color WHEN"
      , option
      , name "color"
      , metavar "WHEN"
      , reader $ eitherReader readLogColor
      ]

setLogLevel :: LogLevel -> LogSettings -> LogSettings
setLogLevel = setLogSettingsLevels . flip newLogLevels []
