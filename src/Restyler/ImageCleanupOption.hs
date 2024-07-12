module Restyler.ImageCleanupOption
  ( ImageCleanupOption (..)
  , HasImageCleanupOption (..)
  , toImageCleanupOption
  , unImageCleanupOption
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
