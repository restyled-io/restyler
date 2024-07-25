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
import Restyler.Options.FailOnDifferences
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.LogSettings
import Restyler.Options.Manifest
import Restyler.Options.NoCommit
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettingsOption
  , failOnDifferences :: FailOnDifferencesOption
  , hostDirectory :: HostDirectoryOption
  , noCommit :: NoCommitOption
  , restrictions :: Restrictions
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Options)
  deriving (HasManifestOption) via (NoManifestOption Options)
  deriving (HasImageCleanupOption) via (NoImageCleanupOption Options)

instance HasFailOnDifferencesOption Options where
  getFailOnDifferencesOption = (.failOnDifferences)

instance HasHostDirectoryOption Options where
  getHostDirectoryOption = (.hostDirectory)

instance HasNoCommitOption Options where
  getNoCommitOption = (.noCommit)

instance HasRestrictions Options where
  getRestrictions = (.restrictions)

envParser :: Env.Parser Env.Error Options
envParser =
  Options
    <$> envLogSettingsOption
    <*> envFailOnDifferences
    <*> envHostDirectoryOption
    <*> envNoCommit
    <*> envRestrictions

optParser :: Parser Options
optParser =
  Options
    <$> optLogSettingsOption
    <*> optFailOnDifferences
    <*> optHostDirectoryOption
    <*> optNoCommit
    <*> pure mempty -- Restrictions are ENV-only

class HasOptions a where
  getOptions :: a -> Options

instance HasOptions Options where
  getOptions = id

newtype ThroughOptions a = ThroughOptions
  { unwrap :: a
  }
  deriving newtype (HasOptions)

instance HasOptions a => HasFailOnDifferencesOption (ThroughOptions a) where
  getFailOnDifferencesOption = getFailOnDifferencesOption . getOptions

instance HasOptions a => HasHostDirectoryOption (ThroughOptions a) where
  getHostDirectoryOption = getHostDirectoryOption . getOptions

instance HasOptions a => HasImageCleanupOption (ThroughOptions a) where
  getImageCleanupOption = getImageCleanupOption . getOptions

instance HasOptions a => HasManifestOption (ThroughOptions a) where
  getManifestOption = getManifestOption . getOptions

instance HasOptions a => HasNoCommitOption (ThroughOptions a) where
  getNoCommitOption = getNoCommitOption . getOptions

instance HasOptions a => HasRestrictions (ThroughOptions a) where
  getRestrictions = getRestrictions . getOptions
