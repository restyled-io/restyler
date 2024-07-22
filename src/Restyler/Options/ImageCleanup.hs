module Restyler.Options.ImageCleanup
  ( ImageCleanupOption (..)
  , HasImageCleanupOption (..)
  , getImageCleanup
  , toImageCleanupOption
  , NoImageCleanupOption (..)
  ) where

import Restyler.Prelude hiding (Last (..))

import Data.Semigroup (Last (..))

newtype ImageCleanupOption = ImageCleanupOption (Last Bool)
  deriving newtype (Semigroup)

class HasImageCleanupOption a where
  getImageCleanupOption :: a -> ImageCleanupOption

getImageCleanup :: (MonadReader env m, HasImageCleanupOption env) => m Bool
getImageCleanup = asks $ unImageCleanupOption . getImageCleanupOption

toImageCleanupOption :: Bool -> ImageCleanupOption
toImageCleanupOption = ImageCleanupOption . Last

unImageCleanupOption :: ImageCleanupOption -> Bool
unImageCleanupOption (ImageCleanupOption x) = getLast x

newtype NoImageCleanupOption a = NoImageCleanupOption
  { unwrap :: a
  }

instance HasImageCleanupOption (NoImageCleanupOption a) where
  getImageCleanupOption = const $ toImageCleanupOption False
