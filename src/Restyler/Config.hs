{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Restyler.Config
    ( Config(..)
    , defaultConfig
    , configPath
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Restyler.Config.ExpectedKeys
import Restyler.Config.Statuses
import Restyler.RemoteFile
import Restyler.Restyler

-- | Top-level configuration object
data Config = Config
    { cEnabled :: Bool
    -- ^ Do anything at all?
    , cAuto :: Bool
    -- ^ Just push the restyling, don't comment?
    , cRemoteFiles :: [RemoteFile]
    -- ^ Any remote configuration files to fetch before restyling
    , cCommentsEnabled :: Bool
    -- ^ Leave Comments?
    , cStatuses :: Statuses
    -- ^ Send PR statuses?
    , cRestylers :: [Restyler]
    -- ^ What restylers to run
    }
    deriving (Eq, Show, Generic)

instance FromJSON Config where
    parseJSON (Array v) = do
        restylers <- mapM parseJSON (V.toList v)
        pure defaultConfig { cRestylers = restylers }
    parseJSON (Object o) = do
        validateObjectKeys
            ["enabled", "auto", "remote_files", "comments", "statuses", "restylers"] o
        Config
            <$> o .:? "enabled" .!= cEnabled defaultConfig
            <*> o .:? "auto" .!= cAuto defaultConfig
            <*> o .:? "remote_files" .!= cRemoteFiles defaultConfig
            <*> o .:? "comments" .!= cCommentsEnabled defaultConfig
            <*> o .:? "statuses" .!= cStatuses defaultConfig
            <*> o .:? "restylers" .!= cRestylers defaultConfig
    parseJSON v = typeMismatch "Config object or list of restylers" v

instance ToJSON Config where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Default configuration
--
-- - Enabled
-- - Not Auto
-- - Leave comments
-- - Send statuses
-- - Run most restylers
--
defaultConfig :: Config
defaultConfig = Config
    { cEnabled = True
    , cAuto = False
    , cRemoteFiles = []
    , cCommentsEnabled = True
    , cStatuses = defaultStatusesConfig
    , cRestylers = defaultRestylers
    }

-- | @.restyled.yaml@
configPath :: FilePath
configPath = ".restyled.yaml"
