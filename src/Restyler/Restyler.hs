{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Restyler.Restyler
    ( Restyler(..)
    , getAllRestylersVersioned

    -- * Exported for testing
    , upgradeEnabled
    ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Yaml (decodeFileThrow)
import Restyler.App.Class
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Delimited
import Restyler.RemoteFile

data Restyler = Restyler
    { rEnabled :: Bool
    , rName :: String
    , rImage :: String
    , rCommand :: [String]
    , rArguments :: [String]
    , rInclude :: [Include]
    , rInterpreters :: [Interpreter]
    , rDelimiters :: Maybe Delimiters
    , rSupportsArgSep :: Bool
    , rSupportsMultiplePaths :: Bool
    , rDocumentation :: [String]
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Restyler where
    parseJSON = genericParseJSON (aesonPrefix snakeCase) . upgradeEnabled

-- | Upgrade values from @restylers.yaml@ that lack an @enabled@ key
--
-- Hard-code a value from the list based on the default configuration present
-- here before such a key existed.
--
upgradeEnabled :: Value -> Value
upgradeEnabled = \case
    Object km ->
        let
            mName = KeyMap.lookup "name" km
            enabled = maybe True (`notElem` disabledRestylers) mName
            updated = KeyMap.singleton "enabled" $ Bool enabled
        in Object $ KeyMap.unionWith (\_ x -> x) updated km
    v -> v
  where
    disabledRestylers =
        ["brittany", "google-java-format", "hindent", "hlint", "jdt"]

instance ToJSON Restyler where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

getAllRestylersVersioned
    :: (MonadIO m, MonadLogger m, MonadDownloadFile m) => String -> m [Restyler]
getAllRestylersVersioned version = do
    downloadRemoteFile restylers
    decodeFileThrow $ rfPath restylers
  where
    restylers = RemoteFile
        { rfUrl = URL $ pack $ restylersYamlUrl version
        , rfPath = "/tmp/restylers-" <> version <> ".yaml"
        }

restylersYamlUrl :: String -> String
restylersYamlUrl version =
    "https://docs.restyled.io/data-files/restylers/manifests/"
        <> version
        <> "/restylers.yaml"
