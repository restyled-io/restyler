module Restyler.ManifestOption
  ( ManifestOption (..)
  , HasManifestOption (..)
  , toManifestOption
  , unManifestOption
  , noManifestOptionL
  ) where

import Restyler.Prelude

newtype ManifestOption = ManifestOption (Last FilePath)
  deriving newtype (Semigroup, Monoid)

class HasManifestOption env where
  manifestOptionL :: Lens' env ManifestOption

toManifestOption :: Maybe FilePath -> ManifestOption
toManifestOption = ManifestOption . Last

unManifestOption :: ManifestOption -> Maybe FilePath
unManifestOption (ManifestOption x) = getLast x

noManifestOptionL :: Lens' a ManifestOption
noManifestOptionL = lens (const $ toManifestOption Nothing) const
