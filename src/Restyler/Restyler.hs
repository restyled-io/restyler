{-# LANGUAGE FieldSelectors #-}

-- |
--
-- Module      : Restyler.Restyler
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Restyler
  ( Restyler (..)
  , RestylerRunStyle (..)
  , getAllRestylersVersioned

    -- * Exported for testing
  , upgradeEnabled
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Yaml (decodeFileThrow)
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Config.Manifest
import Restyler.Config.RemoteFile
import Restyler.Delimited
import Restyler.Monad.Directory
import Restyler.Monad.DownloadFile

data Restyler = Restyler
  { rEnabled :: Bool
  , rName :: String
  , rImage :: String
  , rCommand :: [String]
  , rArguments :: [String]
  , rInclude :: [Include]
  , rInterpreters :: [Interpreter]
  , rDelimiters :: Maybe Delimiters
  , rDocumentation :: [String]
  , rRunStyle :: RestylerRunStyle
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON Restyler where
  parseJSON =
    genericParseJSON (aesonPrefix snakeCase)
      . fixRunStyle
      . upgradeEnabled

-- | Look for legacy keys related to @run_style@ and convert them
fixRunStyle :: Value -> Value
fixRunStyle = unionObjectBy $ \km ->
  let result =
        ( fromMaybe (Bool False) $ KeyMap.lookup "run_as_filter" km
        , fromMaybe (Bool True) $ KeyMap.lookup "supports_multiple_paths" km
        , fromMaybe (Bool True) $ KeyMap.lookup "supports_arg_sep" km
        )
  in  case result of
        (Bool True, _, _) -> mkKeyMap RestylerRunStylePathToStdout
        (_, Bool True, Bool False) -> mkKeyMap RestylerRunStylePathsOverwrite
        (_, Bool True, Bool True) -> mkKeyMap RestylerRunStylePathsOverwriteSep
        (_, Bool False, Bool False) -> mkKeyMap RestylerRunStylePathOverwrite
        (_, Bool False, Bool True) -> mkKeyMap RestylerRunStylePathOverwriteSep
        _ -> KeyMap.empty
 where
  mkKeyMap :: ToJSON a => a -> KeyMap Value
  mkKeyMap = KeyMap.singleton "run_style" . toJSON

-- | Upgrade values from @restylers.yaml@ that lack an @enabled@ key
--
-- Hard-code a value from the list based on the default configuration present
-- here before such a key existed.
upgradeEnabled :: Value -> Value
upgradeEnabled = unionObjectBy $ \km ->
  let
    mName = KeyMap.lookup "name" km
    enabled = maybe True (`notElem` disabledRestylers) mName
  in
    KeyMap.singleton "enabled" $ Bool enabled
 where
  disabledRestylers :: [Value]
  disabledRestylers =
    ["brittany", "google-java-format", "hindent", "hlint", "jdt"]

unionObjectBy :: (KeyMap Value -> KeyMap Value) -> Value -> Value
unionObjectBy f = \case
  Object km ->
    let updated = f km
    in  Object $ KeyMap.unionWith (\_ x -> x) updated km
  v -> v

instance ToJSON Restyler where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

data RestylerRunStyle
  = -- | @for input in ...; do ./auto-formatter input > output; done@
    RestylerRunStylePathToStdout
  | -- | @./auto-formatter input1 input2 ...@
    RestylerRunStylePathsOverwrite
  | -- | @./auto-formatter -- input1 input2 ...@
    RestylerRunStylePathsOverwriteSep
  | -- | @for input in ...; do ./auto-formatter input; done@
    RestylerRunStylePathOverwrite
  | -- | @for input in ...; do ./auto-formatter -- input; done@
    RestylerRunStylePathOverwriteSep
  deriving stock (Bounded, Enum, Eq, Generic, Show)

instance FromJSON RestylerRunStyle where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON RestylerRunStyle where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

getAllRestylersVersioned
  :: ( HasManifest env
     , MonadDirectory m
     , MonadDownloadFile m
     , MonadIO m
     , MonadReader env m
     )
  => String
  -> m [Restyler]
getAllRestylersVersioned version = do
  mManifest <- asks getManifest
  case mManifest of
    Nothing -> do
      exists <- doesFileExist restylers.path
      unless exists $ downloadFile restylers.url restylers.path
      decodeFileThrow $ restylers.path
    Just path -> decodeFileThrow $ toFilePath path
 where
  restylers =
    RemoteFile
      { url = restylersYamlUrl version
      , path = "/tmp/restylers-" <> version <> ".yaml"
      }

restylersYamlUrl :: String -> String
restylersYamlUrl version =
  "https://github.com/restyled-io/restylers/releases/download/"
    <> version
    <> "/restylers.yaml"
