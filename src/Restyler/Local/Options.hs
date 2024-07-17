module Restyler.Local.Options
  ( Options (..)
  , HasOptions (..)
  , envParser
  , optParser

    -- * @DerivingVia@
  , ThroughOptions (..)
  ) where

import Restyler.Prelude

import Data.Semigroup.Generic
import Env qualified
import Options.Applicative
import Restyler.HostDirectoryOption
import Restyler.ImageCleanupOption
import Restyler.LogSettingsOption
import Restyler.Options.Manifest
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettingsOption
  , restrictions :: Restrictions
  , hostDirectory :: HostDirectoryOption
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Options)

class HasOptions a where
  getOptions :: a -> Options

instance HasOptions Options where
  getOptions = id

instance HasRestrictions Options where
  getRestrictions = (.restrictions)

instance HasHostDirectoryOption Options where
  getHostDirectoryOption = (.hostDirectory)

newtype ThroughOptions a = ThroughOptions
  { unwrap :: a
  }
  deriving newtype (HasOptions)
  deriving (HasManifestOption) via (NoManifestOption (ThroughOptions a))
  deriving (HasImageCleanupOption) via (NoImageCleanupOption (ThroughOptions a))

instance HasOptions a => HasRestrictions (ThroughOptions a) where
  getRestrictions = getRestrictions . getOptions

instance HasOptions a => HasHostDirectoryOption (ThroughOptions a) where
  getHostDirectoryOption = getHostDirectoryOption . getOptions

envParser :: Env.Parser Env.Error Options
envParser =
  Options
    <$> envLogSettingsOption
    <*> envRestrictions
    <*> envHostDirectoryOption

optParser :: Parser Options
optParser =
  Options
    <$> optLogSettingsOption
    <*> optRestrictions
    <*> optHostDirectoryOption
