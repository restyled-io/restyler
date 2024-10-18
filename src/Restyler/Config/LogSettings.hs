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
  ( LogSettings
  , logSettingsOptionParser
  )
where

import Restyler.Prelude

import Blammo.Logging.LogSettings
import Blammo.Logging.LogSettings.LogLevels
import OptEnvConf

logSettingsOptionParser :: Parser (LogSettings -> LogSettings)
logSettingsOptionParser =
  appEndo . getDual . foldMap (Dual . Endo) <$> fs
 where
  fs = traverse (<|> pure id) [debugParser, traceParser, colorParser]

debugParser :: Parser (LogSettings -> LogSettings)
debugParser =
  setting
    [ help "Enable debug logging"
    , switch (setLogLevel LevelDebug)
    , long "debug"
    ]

traceParser :: Parser (LogSettings -> LogSettings)
traceParser =
  setting
    [ help "Enable trace logging"
    , switch (setLogLevel $ LevelOther "trace")
    , long "trace"
    ]

colorParser :: Parser (LogSettings -> LogSettings)
colorParser =
  setting
    [ help "Enabled color WHEN"
    , option
    , long "color"
    , metavar "WHEN"
    , reader $ eitherReader readSetLogColor
    ]

setLogLevel :: LogLevel -> LogSettings -> LogSettings
setLogLevel = setLogSettingsLevels . flip newLogLevels []

readSetLogColor :: String -> Either String (LogSettings -> LogSettings)
readSetLogColor arg = do
  logColor <- readLogColor arg
  pure $ setLogSettingsColor logColor
