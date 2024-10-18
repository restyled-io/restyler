{-# LANGUAGE TemplateHaskell #-}

module Restyler.Config.Parse
  ( Config' (..)
  , parseConfig
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
import Restyler.Config.Exclude as X
import Restyler.Config.Glob
import Restyler.Config.RemoteFile as X
import Restyler.Config.Restyler as X
import Restyler.Options
import System.IO (hClose)
import UnliftIO.Temporary (withSystemTempFile)

data Config' = Config'
  { enabled :: Bool
  , exclude :: [Glob FilePath]
  , commitTemplate :: CommitTemplate
  , remoteFiles :: [RemoteFile]
  , ignoreAuthors :: [Glob Text]
  , ignoreLabels :: [Glob Text]
  , ignoreBranches :: [Glob Text]
  , restylersVersion :: String
  , restylerOverrides :: [RestylerOverride]
  , options :: Options
  }

instance HasExclude Config' where
  getExclude = (.exclude)

instance HasCommitTemplate Config' where
  getCommitTemplate = (.commitTemplate)

instance HasRemoteFiles Config' where
  getRemoteFiles = (.remoteFiles)

-- HasIgnores...

instance HasRestylersVersion Config' where
  getRestylersVersion = (.restylersVersion)

instance HasRestylerOverrides Config' where
  getRestylerOverrides = (.restylerOverrides)

parseConfig :: IO Config'
parseConfig = do
  withSystemTempFile "restyler-default-config.yaml" $ \tmp h -> do
    BS.hPutStr h defaultConfigContent >> hClose h
    runParser Pkg.version "Restyle local file" $ configParser tmp

defaultConfigContent :: ByteString
defaultConfigContent = $(embedFile "config/default.yaml")

configParser :: FilePath -> Parser Config'
configParser defaults =
  withCombinedYamlConfigs (configSources defaults)
    $ Config'
    <$> setting
      [ help "Do anything at all"
      , conf "enabled"
      ]
    <*> excludeParser
    <*> commitTemplateParser
    <*> remoteFilesParser
    <*> pure [] -- TODO
    <*> pure [] -- TODO
    <*> pure [] -- TODO
    <*> setting
      [ help "Version of Restylers manifest to use"
      , option
      , long "restylers-version"
      , reader str
      , metavar "stable|dev|..."
      , env "RESTYLERS_VERSION"
      , conf "restylers_version"
      ]
    <*> restylerOverridesParser
    <*> subConfig_ "cli" optionsParser

configSources :: FilePath -> Parser [Path Abs File]
configSources defaults =
  sequenceA
    [ hiddenPath ".github/restyled.yml"
    , hiddenPath ".github/restyled.yaml"
    , hiddenPath ".restyled.yml"
    , hiddenPath ".restyled.yaml"
    , hiddenPath defaults
    ]

hiddenPath :: FilePath -> Parser (Path Abs File)
hiddenPath x = filePathSetting [value x, hidden]

class HasConfig a where
  getConfig :: a -> Config'

instance HasConfig Config' where
  getConfig = id

newtype ThroughConfig a = ThroughConfig
  { unwrap :: a
  }
  deriving newtype (HasConfig)

instance HasConfig a => HasExclude (ThroughConfig a) where
  getExclude = getExclude . getConfig

instance HasConfig a => HasCommitTemplate (ThroughConfig a) where
  getCommitTemplate = getCommitTemplate . getConfig

instance HasConfig a => HasRemoteFiles (ThroughConfig a) where
  getRemoteFiles = getRemoteFiles . getConfig

instance HasConfig a => HasRestylersVersion (ThroughConfig a) where
  getRestylersVersion = getRestylersVersion . getConfig

instance HasConfig a => HasRestylerOverrides (ThroughConfig a) where
  getRestylerOverrides = getRestylerOverrides . getConfig
