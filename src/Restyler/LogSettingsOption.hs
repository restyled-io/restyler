module Restyler.LogSettingsOption
  ( LogSettingsOption (..)
  , resolveLogSettings
  , envLogSettingsOption
  , optLogSettingsOption
  )
where

import Restyler.Prelude

import Blammo.Logging.LogSettings.Env qualified as LogSettingsEnv
import Env qualified
import Options.Applicative

newtype LogSettingsOption = LogSettingsOption (Endo LogSettings)
  deriving newtype (Semigroup)

toLogSettingsOption :: LogSettings -> LogSettingsOption
toLogSettingsOption = LogSettingsOption . Endo . const

resolveLogSettings :: LogSettingsOption -> LogSettings
resolveLogSettings (LogSettingsOption f) = appEndo f defaultLogSettings

envLogSettingsOption :: Env.Parser Env.Error LogSettingsOption
envLogSettingsOption = toLogSettingsOption <$> LogSettingsEnv.parser

-- TODO
-- @--color=never|always|auto@
-- @--debug@
optLogSettingsOption :: Parser LogSettingsOption
optLogSettingsOption = pure $ LogSettingsOption $ Endo id
