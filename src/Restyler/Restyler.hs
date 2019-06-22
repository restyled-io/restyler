module Restyler.Restyler
    ( Restyler(..)
    , getAllRestylersVersioned
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Yaml (decodeFileThrow)
import Restyler.App.Class
import Restyler.Config.ExpectedKeys
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.RemoteFile

data Restyler = Restyler
    { rName :: String
    , rImage :: String
    , rCommand :: [String]
    , rArguments :: [String]
    , rInclude :: [Include]
    , rInterpreters :: [Interpreter]
    , rSupportsArgSep :: Bool
    , rSupportsMultiplePaths :: Bool
    , rDocumentation :: [String]
    }
    deriving (Eq, Show, Generic)

instance FromJSON Restyler where
    parseJSON = genericParseJSONValidated $ aesonPrefix snakeCase

instance ToJSON Restyler where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

getAllRestylersVersioned
    :: (HasLogFunc env, HasSystem env, HasDownloadFile env)
    => String
    -> RIO env [Restyler]
getAllRestylersVersioned version = do
    -- Should downloadRemoteFile handle "unless exists" itself?
    unlessM (doesFileExist $ rfPath restylers) $ downloadRemoteFile restylers
    decodeFileThrow $ rfPath restylers
  where
    restylers = RemoteFile
        { rfUrl = URL $ pack $ restylersYamlUrl version
        , rfPath = "/tmp/restylers-" <> version <> ".yaml"
        }

restylersYamlUrl :: String -> String
restylersYamlUrl version =
    "https://raw.githubusercontent.com/restyled-io/restylers/"
        <> version
        <> "/restylers.yaml"
