module Restyler.ImageCleanupOption
  ( ImageCleanupOption (..)
  , HasImageCleanupOption (..)
  , toImageCleanupOption
  , unImageCleanupOption
  , noImageCleanupOptionL
  ) where

import Restyler.Prelude hiding (Last (..))

import Data.Semigroup (Last (..))

newtype ImageCleanupOption = ImageCleanupOption (Last Bool)
  deriving newtype (Semigroup)

class HasImageCleanupOption env where
  imageCleanupOptionL :: Lens' env ImageCleanupOption

toImageCleanupOption :: Bool -> ImageCleanupOption
toImageCleanupOption = ImageCleanupOption . Last

unImageCleanupOption :: ImageCleanupOption -> Bool
unImageCleanupOption (ImageCleanupOption x) = getLast x

noImageCleanupOptionL :: Lens' a ImageCleanupOption
noImageCleanupOptionL = lens (const $ toImageCleanupOption False) const
