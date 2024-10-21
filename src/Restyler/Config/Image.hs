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

import Restyler.Prelude hiding (First (..), (.=))

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Semigroup (First (..))
import Data.Semigroup.Generic
import Data.Text qualified as T

data Image = Image
  { registry :: Maybe (First Text)
  , name :: Maybe (First Text)
  , tag :: Maybe (First Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Image)
  deriving (FromJSON, ToJSON) via (Autodocodec Image)

instance HasCodec Image where
  codec = parseAlternatives codecImage [codecObject]

codecImage :: JSONCodec Image
codecImage =
  bimapCodec (Right . imageFromText) imageToText textCodec
    <?> "[<registry>/]<name>:<tag>"

codecObject :: JSONCodec Image
codecObject =
  object
    "Image"
    $ Image
    <$> (optionalField' "registry" .= (.registry))
    <*> (optionalField' "name" .= (.name))
    <*> (optionalField' "tag" .= (.tag))

imageFromText :: Text -> Image
imageFromText base = case safeBreakOnStrip "/" base of
  Nothing -> case safeBreakOnStrip ":" base of
    Nothing ->
      Image
        { registry = Nothing
        , name = Just $ First base
        , tag = Nothing
        }
    Just (name, tag) ->
      Image
        { registry = Nothing
        , name = Just $ First name
        , tag = Just $ First tag
        }
  Just (registry, rest) -> case safeBreakOnStrip ":" rest of
    Nothing ->
      Image
        { registry = Just $ First registry
        , name = Just $ First rest
        , tag = Nothing
        }
    Just (name, tag) ->
      Image
        { registry = Just $ First registry
        , name = Just $ First name
        , tag = Just $ First tag
        }

imageToText :: Image -> Text
imageToText Image {..} =
  mconcat
    [ maybe "" ((<> "/") . getFirst) registry
    , maybe "" getFirst name
    , maybe "" ((":" <>) . getFirst) tag
    ]

overrideRestylerImage :: String -> Image -> String
overrideRestylerImage base = unpack . imageToText . (<> imageFromText (pack base))

safeBreakOnStrip :: Text -> Text -> Maybe (Text, Text)
safeBreakOnStrip x = bitraverse pure (T.stripPrefix x) . T.breakOn x
