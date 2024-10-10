-- |
--
-- Module      : Restyler.Local.Options
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
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
import Restyler.Options.DryRun
import Restyler.Options.FailOnDifferences
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.LogSettings
import Restyler.Options.Manifest
import Restyler.Options.NoClean
import Restyler.Options.NoCommit
import Restyler.Options.NoPull
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettingsOption
  , dryRun :: DryRunOption
  , failOnDifferences :: FailOnDifferencesOption
  , hostDirectory :: HostDirectoryOption
  , imageCleanup :: ImageCleanupOption
  , manifest :: ManifestOption
  , noCommit :: NoCommitOption
  , noClean :: NoCleanOption
  , noPull :: NoPullOption
  , restrictions :: Restrictions
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Options)

instance HasDryRunOption Options where
  getDryRunOption = (.dryRun)

instance HasFailOnDifferencesOption Options where
  getFailOnDifferencesOption = (.failOnDifferences)

instance HasHostDirectoryOption Options where
  getHostDirectoryOption = (.hostDirectory)

instance HasImageCleanupOption Options where
  getImageCleanupOption = (.imageCleanup)

instance HasManifestOption Options where
  getManifestOption = (.manifest)

instance HasNoCommitOption Options where
  getNoCommitOption = (.noCommit)

instance HasNoCleanOption Options where
  getNoCleanOption = (.noClean)

instance HasNoPullOption Options where
  getNoPullOption = (.noPull)

instance HasRestrictions Options where
  getRestrictions = (.restrictions)

envParser :: Env.Parser Env.Error Options
envParser =
  Options
    <$> envLogSettingsOption
    <*> envDryRunOption
    <*> envFailOnDifferencesOption
    <*> envHostDirectoryOption
    <*> envImageCleanupOption
    <*> envManifestOption
    <*> envNoCommitOption
    <*> envNoCleanOption
    <*> envNoPullOption
    <*> envRestrictions

optParser :: Parser Options
optParser =
  Options
    <$> optLogSettingsOption
    <*> optDryRunOption
    <*> optFailOnDifferencesOption
    <*> optHostDirectoryOption
    <*> optImageCleanupOption
    <*> optManifestOption
    <*> optNoCommitOption
    <*> optNoCleanOption
    <*> optNoPullOption
    <*> pure mempty -- Restrictions are ENV-only

class HasOptions a where
  getOptions :: a -> Options

instance HasOptions Options where
  getOptions = id

newtype ThroughOptions a = ThroughOptions
  { unwrap :: a
  }
  deriving newtype (HasOptions)

instance HasOptions a => HasDryRunOption (ThroughOptions a) where
  getDryRunOption = getDryRunOption . getOptions

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

instance HasOptions a => HasNoCleanOption (ThroughOptions a) where
  getNoCleanOption = getNoCleanOption . getOptions

instance HasOptions a => HasNoPullOption (ThroughOptions a) where
  getNoPullOption = getNoPullOption . getOptions

instance HasOptions a => HasRestrictions (ThroughOptions a) where
  getRestrictions = getRestrictions . getOptions
