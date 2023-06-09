module Restyler.Config.SketchyList
  ( SketchyList
  , unSketchy
  ) where

import Prelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)

data SketchyList a = One a | Many [a]
  deriving stock (Eq, Show)

instance Semigroup (SketchyList a) where
  a <> b = Many $ unSketchy a <> unSketchy b

unSketchy :: SketchyList a -> [a]
unSketchy (One i) = [i]
unSketchy (Many is) = is

instance FromJSON a => FromJSON (SketchyList a) where
  parseJSON i@(String _) = One <$> parseJSON i
  parseJSON is@(Array _) = Many <$> parseJSON is
  parseJSON x = typeMismatch "item or list of items" x
