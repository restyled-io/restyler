{-# LANGUAGE TemplateHaskell #-}

-- | Handling of @.restyled.yaml@ content and behavior driven there-by
--
-- Module      : Restyler.Config
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config
  ( Config (..)
  , configParser
  , parseConfig

    -- * Individual configuration points
  , HasEnabled (..)
  , module X

    -- * @DerivingVia@
  , HasConfig (..)
  , ThroughConfig (..)
  ) where

import Restyler.Prelude

import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import OptEnvConf
import Paths_restyler qualified as Pkg
import Restyler.Config.CommitTemplate as X
import Restyler.Config.DryRun as X
import Restyler.Config.Exclude as X
import Restyler.Config.FailOnDifferences as X
import Restyler.Config.Glob
import Restyler.Config.HostDirectory as X
import Restyler.Config.Ignore as X
import Restyler.Config.ImageCleanup as X
import Restyler.Config.LogSettings as X
import Restyler.Config.Manifest as X
import Restyler.Config.NoClean as X
import Restyler.Config.NoCommit as X
import Restyler.Config.NoPull as X
import Restyler.Config.RemoteFile as X
import Restyler.Config.Restrictions as X
import Restyler.Config.Restyler as X
import System.IO (hClose)
import UnliftIO.Temporary (withSystemTempFile)

data Config = Config
  { logSettings :: LogSettings -> LogSettings
  , enabled :: Bool
  , dryRun :: Bool
  , failOnDifferences :: Bool
  , exclude :: [Glob FilePath]
  , commitTemplate :: CommitTemplate
  , remoteFiles :: [RemoteFile]
  , ignores :: Ignores
  , restylersVersion :: String
  , restylersManifest :: Maybe (Path Abs File)
  , restylerOverrides :: [RestylerOverride]
  , hostDirectory :: Path Abs Dir
  , imageCleanup :: Bool
  , noPull :: Bool
  , restrictions :: Restrictions
  , noCommit :: Bool
  , noClean :: Bool
  , pullRequestJson :: Maybe (Path Abs File)
  , paths :: NonEmpty FilePath
  }

class HasEnabled env where
  getEnabled :: env -> Bool

instance HasCommitTemplate Config where getCommitTemplate = (.commitTemplate)
instance HasDryRun Config where getDryRun = (.dryRun)
instance HasEnabled Config where getEnabled = (.enabled)
instance HasExclude Config where getExclude = (.exclude)
instance HasFailOnDifferences Config where
  getFailOnDifferences = (.failOnDifferences)
instance HasHostDirectory Config where getHostDirectory = (.hostDirectory)
instance HasIgnores Config where getIgnores = (.ignores)
instance HasImageCleanup Config where getImageCleanup = (.imageCleanup)
instance HasManifest Config where getManifest = (.restylersManifest)
instance HasNoClean Config where getNoClean = (.noClean)
instance HasNoCommit Config where getNoCommit = (.noCommit)
instance HasNoPull Config where getNoPull = (.noPull)
instance HasRemoteFiles Config where getRemoteFiles = (.remoteFiles)
instance HasRestrictions Config where getRestrictions = (.restrictions)
instance HasRestylerOverrides Config where
  getRestylerOverrides = (.restylerOverrides)
instance HasRestylersVersion Config where
  getRestylersVersion = (.restylersVersion)

parseConfig :: IO Config
parseConfig = do
  withSystemTempFile "restyler-default-config.yaml" $ \defaults h -> do
    BS.hPutStr h defaultConfigContent >> hClose h
    runParser Pkg.version "Restyle local file"
      $ configParser
        [ ".github/restyled.yml"
        , ".github/restyled.yaml"
        , ".restyled.yml"
        , ".restyled.yaml"
        , defaults
        ]

defaultConfigContent :: ByteString
defaultConfigContent = $(embedFile "config/default.yaml")

configParser :: [FilePath] -> Parser Config
configParser paths =
  withCombinedYamlConfigs (traverse hiddenPath paths)
    $ Config
    <$> logSettingsOptionParser
    <*> setting
      [ help "Do anything at all"
      , conf "enabled"
      ]
    <*> dryRunParser
    <*> failOnDifferencesParser
    <*> excludeParser
    <*> commitTemplateParser
    <*> remoteFilesParser
    <*> ignoresParser
    <*> setting
      [ help "Version of Restylers manifest to use"
      , option
      , long "restylers-version"
      , reader str
      , metavar "stable|dev|..."
      , env "RESTYLERS_VERSION"
      , conf "restylers_version"
      ]
    <*> optional manifestParser
    <*> restylerOverridesParser
    <*> subConfig_ "docker" hostDirectoryParser
    <*> subConfig_ "docker" noPullParser
    <*> subConfig_ "docker" imageCleanupParser
    <*> subConfig_ "docker" restrictionsParser
    <*> subConfig_ "git" noCommitParser
    <*> subConfig_ "git" noCleanParser
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

hiddenPath :: FilePath -> Parser (Path Abs File)
hiddenPath x = filePathSetting [value x, hidden]

class HasConfig a where
  getConfig :: a -> Config

instance HasConfig Config where
  getConfig = id

newtype ThroughConfig a = ThroughConfig
  { unwrap :: a
  }
  deriving newtype (HasConfig)

instance HasConfig a => HasCommitTemplate (ThroughConfig a) where
  getCommitTemplate = getCommitTemplate . getConfig
instance HasConfig a => HasDryRun (ThroughConfig a) where
  getDryRun = getDryRun . getConfig
instance HasConfig a => HasEnabled (ThroughConfig a) where
  getEnabled = getEnabled . getConfig
instance HasConfig a => HasExclude (ThroughConfig a) where
  getExclude = getExclude . getConfig
instance HasConfig a => HasFailOnDifferences (ThroughConfig a) where
  getFailOnDifferences = getFailOnDifferences . getConfig
instance HasConfig a => HasHostDirectory (ThroughConfig a) where
  getHostDirectory = getHostDirectory . getConfig
instance HasConfig a => HasIgnores (ThroughConfig a) where
  getIgnores = getIgnores . getConfig
instance HasConfig a => HasImageCleanup (ThroughConfig a) where
  getImageCleanup = getImageCleanup . getConfig
instance HasConfig a => HasManifest (ThroughConfig a) where
  getManifest = getManifest . getConfig
instance HasConfig a => HasNoClean (ThroughConfig a) where
  getNoClean = getNoClean . getConfig
instance HasConfig a => HasNoCommit (ThroughConfig a) where
  getNoCommit = getNoCommit . getConfig
instance HasConfig a => HasNoPull (ThroughConfig a) where
  getNoPull = getNoPull . getConfig
instance HasConfig a => HasRemoteFiles (ThroughConfig a) where
  getRemoteFiles = getRemoteFiles . getConfig
instance HasConfig a => HasRestrictions (ThroughConfig a) where
  getRestrictions = getRestrictions . getConfig
instance HasConfig a => HasRestylerOverrides (ThroughConfig a) where
  getRestylerOverrides = getRestylerOverrides . getConfig
instance HasConfig a => HasRestylersVersion (ThroughConfig a) where
  getRestylersVersion = getRestylersVersion . getConfig
