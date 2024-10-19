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
  , module X
  ) where

import Restyler.Prelude

import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
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
    <*> enabledParser
    <*> dryRunParser
    <*> failOnDifferencesParser
    <*> excludeParser
    <*> commitTemplateParser
    <*> remoteFilesParser
    <*> ignoresParser
    <*> restylersVersionParser
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
