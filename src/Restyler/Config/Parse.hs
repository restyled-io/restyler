{-# LANGUAGE TemplateHaskell #-}

module Restyler.Config.Parse
  ( Config' (..)
  , parseConfig

    -- * @DerivingVia@
  , HasConfig (..)
  , ThroughConfig (..)
  ) where

import Restyler.Prelude

import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import OptEnvConf
import Paths_restyler qualified as Pkg
import Restyler.Config.Glob
import Restyler.Options
import System.IO (hClose)
import UnliftIO.Temporary (withSystemTempFile)

data Config' = Config'
  { enabled :: Bool
  , exclude :: [Glob FilePath]
  , options :: Options
  }

instance HasExclude Config' where
  getExclude = (.exclude)

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
