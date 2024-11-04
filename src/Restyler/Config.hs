{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

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
  , configPaths
  , configParser
  , parseConfig

    -- * Individual configuration points
  , module X
  ) where

import Restyler.Prelude

import OptEnvConf
import Paths_restyler qualified as Pkg
import Restyler.Config.CommitTemplate as X
import Restyler.Config.DryRun as X
import Restyler.Config.Enabled as X
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

data Config = Config
  { enabled :: Bool
  , dryRun :: Bool
  , failOnDifferences :: Bool
  , exclude :: [Glob FilePath]
  , restylersVersion :: String
  , restylersManifest :: Maybe (Path Abs File)
  , restylerOverrides :: [RestylerOverride]
  , ignores :: Ignores
  , remoteFiles :: [RemoteFile]
  , hostDirectory :: Path Abs Dir
  , imageCleanup :: Bool
  , noPull :: Bool
  , restrictions :: Restrictions
  , commitTemplate :: CommitTemplate
  , noCommit :: Bool
  , noClean :: Bool
  , logSettings :: LogSettingsOption
  , pullRequestJson :: Maybe (Path Abs File)
  , paths :: NonEmpty FilePath
  }

parseConfig :: IO Config
parseConfig = runParser Pkg.version "Restyle local files" $ configParser configPaths

configPaths :: [FilePath]
configPaths =
  [ ".github/restyled.yml"
  , ".github/restyled.yaml"
  , ".restyled.yml"
  , ".restyled.yaml"
  ]

configParser :: [FilePath] -> Parser Config
configParser sources =
  withFirstYamlConfig (traverse hiddenPath sources) $ do
    enabled <- enabledParser
    dryRun <- dryRunParser
    failOnDifferences <- failOnDifferencesParser
    restylersVersion <- restylersVersionParser
    restylersManifest <- optional manifestParser
    restylerOverrides <- restylerOverridesParser
    exclude <- excludeParser
    ignores <- ignoresParser
    remoteFiles <- remoteFilesParser
    hostDirectory <- subConfig_ "docker" hostDirectoryParser
    imageCleanup <- subConfig_ "docker" imageCleanupParser
    noPull <- subConfig_ "docker" noPullParser
    restrictions <- subConfig_ "docker" $ subAll "restyler" restrictionsParser
    commitTemplate <- commitTemplateParser
    noCommit <- subConfig_ "git" noCommitParser
    noClean <- subConfig_ "git" noCleanParser
    logSettings <- subConfig_ "logging" logSettingsOptionParser
    pullRequestJson <-
      optional
        $ filePathSetting
          [ help ""
          , hidden
          , option
          , long "pull-request-json"
          ]
    paths <-
      someNonEmpty
        $ setting
          [ help "Path to restyle"
          , argument
          , reader str
          , metavar "PATH"
          ]
    pure Config {..}

-- | Use 'filePathSetting' to handle creating the @'Path' 'Abs' 'File'@ we need
--
-- And mark it 'hidden' so it doesn't appear in docs.
hiddenPath :: FilePath -> Parser (Path Abs File)
hiddenPath x = filePathSetting [value x, hidden]
