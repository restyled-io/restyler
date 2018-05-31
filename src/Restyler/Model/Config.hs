{-# LANGUAGE OverloadedStrings #-}

module Restyler.Model.Config
    ( Config(..)
    , defaultConfig
    , configPath
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Restyler.Model.Restyler
import Restyler.Model.StatusesConfig

-- | Top-level configuration object
data Config = Config
    { cEnabled :: Bool
    -- ^ Do anything at all?
    , cAuto :: Bool
    -- ^ Just push the restyling, don't comment?
    , cStatusesConfig :: StatusesConfig
    -- ^ Send PR statuses?
    , cRestylers :: [Restyler]
    -- ^ What restylers to run
    }
    deriving (Eq, Show)

instance FromJSON Config where
    parseJSON (Array v) = do
        restylers <- mapM parseJSON (V.toList v)
        pure defaultConfig { cRestylers = restylers }
    parseJSON (Object o) = Config
        -- Use default values if un-specified
        <$> o .:? "enabled" .!= cEnabled defaultConfig
        <*> o .:? "auto" .!= cAuto defaultConfig
        <*> o .:? "statuses" .!= cStatusesConfig defaultConfig
        <*> o .:? "restylers" .!= cRestylers defaultConfig
    parseJSON v = typeMismatch "Config object or list of restylers" v

-- | Default configuration
--
-- - Enabled
-- - Not Auto
-- - Send statuses
-- - Run most restylers
--
defaultConfig :: Config
defaultConfig = Config
    { cEnabled = True
    , cAuto = False
    , cStatusesConfig = defaultStatusesConfig
    , cRestylers = defaultRestylers
    }

-- | @.restyled.yaml@
configPath :: FilePath
configPath = ".restyled.yaml"
