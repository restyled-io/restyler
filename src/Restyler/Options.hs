-- |
--
-- Module      : Restyler.Options
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options
  ( Options (..)
  , optionsParser
  , module X

    -- * @DerivingVia@
  , HasOptions (..)
  , ThroughOptions (..)
  ) where

import Restyler.Prelude

import OptEnvConf
import Restyler.Options.DryRun as X
import Restyler.Options.FailOnDifferences as X
import Restyler.Options.HostDirectory as X
import Restyler.Options.ImageCleanup as X
import Restyler.Options.LogSettings as X
import Restyler.Options.Manifest as X
import Restyler.Options.NoClean as X
import Restyler.Options.NoCommit as X
import Restyler.Options.NoPull as X
import Restyler.Options.Restrictions as X

data Options = Options
  { logSettings :: LogSettings -> LogSettings
  , dryRun :: Bool
  , failOnDifferences :: Bool
  , hostDirectory :: Path Abs Dir
  , imageCleanup :: Bool
  , manifest :: Maybe (Path Abs File)
  , noCommit :: Bool
  , noClean :: Bool
  , noPull :: Bool
  , restrictions :: Restrictions
  , pullRequestJson :: Maybe (Path Abs File)
  , paths :: NonEmpty FilePath
  }

instance HasDryRun Options where
  getDryRun = (.dryRun)

instance HasFailOnDifferences Options where
  getFailOnDifferences = (.failOnDifferences)

instance HasHostDirectory Options where
  getHostDirectory = (.hostDirectory)

instance HasImageCleanup Options where
  getImageCleanup = (.imageCleanup)

instance HasManifest Options where
  getManifest = (.manifest)

instance HasNoCommit Options where
  getNoCommit = (.noCommit)

instance HasNoClean Options where
  getNoClean = (.noClean)

instance HasNoPull Options where
  getNoPull = (.noPull)

instance HasRestrictions Options where
  getRestrictions = (.restrictions)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> logSettingsOptionParser
    <*> dryRunParser
    <*> failOnDifferencesParser
    <*> hostDirectoryParser
    <*> imageCleanupParser
    <*> optional manifestParser
    <*> noCommitParser
    <*> noCleanParser
    <*> noPullParser
    <*> restrictionsParser
    <*> optional
      ( filePathSetting
          [ help ""
          , hidden
          , option
          , long "pull-request-json"
          ]
      )
    <*> someNonEmpty
      ( setting
          [ help "Path to restyle"
          , argument
          , reader str
          , metavar "PATH"
          ]
      )

class HasOptions a where
  getOptions :: a -> Options

instance HasOptions Options where
  getOptions = id

newtype ThroughOptions a = ThroughOptions
  { unwrap :: a
  }
  deriving newtype (HasOptions)

instance HasOptions a => HasDryRun (ThroughOptions a) where
  getDryRun = getDryRun . getOptions

instance HasOptions a => HasFailOnDifferences (ThroughOptions a) where
  getFailOnDifferences = getFailOnDifferences . getOptions

instance HasOptions a => HasHostDirectory (ThroughOptions a) where
  getHostDirectory = getHostDirectory . getOptions

instance HasOptions a => HasImageCleanup (ThroughOptions a) where
  getImageCleanup = getImageCleanup . getOptions

instance HasOptions a => HasManifest (ThroughOptions a) where
  getManifest = getManifest . getOptions

instance HasOptions a => HasNoCommit (ThroughOptions a) where
  getNoCommit = getNoCommit . getOptions

instance HasOptions a => HasNoClean (ThroughOptions a) where
  getNoClean = getNoClean . getOptions

instance HasOptions a => HasNoPull (ThroughOptions a) where
  getNoPull = getNoPull . getOptions

instance HasOptions a => HasRestrictions (ThroughOptions a) where
  getRestrictions = getRestrictions . getOptions
