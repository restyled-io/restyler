module Restyler.Options.RestyleLocal
  ( Options (..)
  , envOptions
  , optOptions
  , envParser
  , optParser
  ) where

import Restyler.Prelude

import Data.Semigroup.Generic
import Env qualified
import Options.Applicative
import Restyler.HostDirectoryOption
import Restyler.LogSettingsOption
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettingsOption
  , restrictions :: Restrictions
  , hostDirectory :: HostDirectoryOption
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Options)

instance HasRestrictions Options where
  restrictionsL = lens (.restrictions) $ \x y -> x {restrictions = y}

instance HasHostDirectoryOption Options where
  hostDirectoryOptionL = lens (.hostDirectory) $ \x y -> x {hostDirectory = y}

envOptions :: IO Options
envOptions = Env.parse id envParser

envParser :: Env.Parser Env.Error Options
envParser =
  Options
    <$> envLogSettingsOption
    <*> envRestrictions
    <*> envHostDirectoryOption

optOptions :: IO Options
optOptions = execParser $ info (optParser <**> helper) fullDesc

optParser :: Parser Options
optParser =
  Options
    <$> optLogSettingsOption
    <*> optRestrictions
    <*> optHostDirectoryOption
