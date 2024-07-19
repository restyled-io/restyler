module Restyler.Local.Options
  ( Options (..)
  , envParser
  , optParser

    -- * @DerivingVia@
  , HasOptions (..)
  , ThroughOptions (..)
  ) where

import Restyler.Prelude

import Data.Semigroup.Generic
import Env qualified
import Options.Applicative
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.LogSettings
import Restyler.Options.Manifest
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettingsOption
  , restrictions :: Restrictions
  , hostDirectory :: HostDirectoryOption
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Options)

instance HasRestrictions Options where
  getRestrictions = (.restrictions)

instance HasHostDirectoryOption Options where
  getHostDirectoryOption = (.hostDirectory)

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
    <*> pure mempty -- Restrictions are ENV-only
    <*> optHostDirectoryOption

class HasOptions a where
  getOptions :: a -> Options

instance HasOptions Options where
  getOptions = id

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
