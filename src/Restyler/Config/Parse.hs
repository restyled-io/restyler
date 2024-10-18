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
import Restyler.Options
import System.IO (hClose)
import UnliftIO.Temporary (withSystemTempFile)

data Config' = Config'
  { enabled :: Bool
  , exclude :: [Glob FilePath]
  , commitTemplate :: CommitTemplate
  , remoteFiles :: [RemoteFile]
  , options :: Options
  }

instance HasExclude Config' where
  getExclude = (.exclude)

instance HasCommitTemplate Config' where
  getCommitTemplate = (.commitTemplate)

instance HasRemoteFiles Config' where
  getRemoteFiles = (.remoteFiles)

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
