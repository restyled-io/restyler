{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--
-- Module      : Restyler.Config.Image
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Image
  ( Image
  , overrideRestylerImage
  ) where

import Restyler.Prelude hiding (First (..))

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Semigroup (First (..))
import Data.Semigroup.Generic
import Data.Text qualified as T

data ImageFields = ImageFields
  { registry :: Maybe (First Text)
  , name :: Maybe (First Text)
  , tag :: Maybe (First Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Semigroup) via (GenericSemigroupMonoid ImageFields)

imageFieldsFromText :: Text -> ImageFields
imageFieldsFromText base = case safeBreakOnStrip "/" base of
  Nothing -> case safeBreakOnStrip ":" base of
    Nothing ->
      ImageFields
        { registry = Nothing
        , name = Just $ First base
        , tag = Nothing
        }
    Just (name, tag) ->
      ImageFields
        { registry = Nothing
        , name = Just $ First name
        , tag = Just $ First tag
        }
  Just (registry, rest) -> case safeBreakOnStrip ":" rest of
    Nothing ->
      ImageFields
        { registry = Just $ First registry
        , name = Just $ First rest
        , tag = Nothing
        }
    Just (name, tag) ->
      ImageFields
        { registry = Just $ First registry
        , name = Just $ First name
        , tag = Just $ First tag
        }

safeBreakOnStrip :: Text -> Text -> Maybe (Text, Text)
safeBreakOnStrip x = bitraverse pure (T.stripPrefix x) . T.breakOn x

imageFieldsFromString :: String -> ImageFields
imageFieldsFromString = imageFieldsFromText . pack

imageFieldsToText :: ImageFields -> Text
imageFieldsToText ImageFields {..} =
  mconcat
    [ maybe "" ((<> "/") . getFirst) registry
    , maybe "" getFirst name
    , maybe "" ((":" <>) . getFirst) tag
    ]

imageFieldsToString :: ImageFields -> String
imageFieldsToString = unpack . imageFieldsToText

data Image = Image Text | ImageOverride ImageFields
  deriving stock (Eq, Show, Generic)

instance FromJSON Image where
  parseJSON = \case
    v@String {} -> Image <$> parseJSON v
    v@Object {} -> ImageOverride <$> parseJSON v
    v -> typeMismatch "String or Object" v

instance ToJSON Image where
  toJSON = \case
    Image i -> toJSON i
    ImageOverride fs -> toJSON fs
  toEncoding = \case
    Image i -> toEncoding i
    ImageOverride fs -> toEncoding fs

overrideRestylerImage :: String -> Image -> String
overrideRestylerImage base = \case
  Image i -> unpack i
  ImageOverride fs -> imageFieldsToString $ fs <> imageFieldsFromString base
