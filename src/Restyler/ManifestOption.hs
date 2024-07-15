module Restyler.ManifestOption
  ( ManifestOption (..)
  , HasManifestOption (..)
  , getManifest
  , toManifestOption
  , NoManifestOption (..)
  ) where

import Restyler.Prelude

newtype ManifestOption = ManifestOption (Last FilePath)
  deriving newtype (Semigroup, Monoid)

class HasManifestOption a where
  getManifestOption :: a -> ManifestOption

getManifest :: (MonadReader env m, HasManifestOption env) => m (Maybe FilePath)
getManifest = asks $ unManifestOption . getManifestOption

toManifestOption :: Maybe FilePath -> ManifestOption
toManifestOption = ManifestOption . Last

unManifestOption :: ManifestOption -> Maybe FilePath
unManifestOption (ManifestOption x) = getLast x

newtype NoManifestOption a = NoManifestOption
  { unwrap :: a
  }

instance HasManifestOption (NoManifestOption a) where
  getManifestOption = const $ toManifestOption Nothing
