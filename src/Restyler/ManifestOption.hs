module Restyler.ManifestOption
  ( ManifestOption (..)
  , HasManifestOption (..)
  , toManifestOption
  , unManifestOption
  ) where

import Restyler.Prelude

newtype ManifestOption = ManifestOption (Last FilePath)
  deriving newtype (Semigroup, Monoid)

toManifestOption :: Maybe FilePath -> ManifestOption
toManifestOption = ManifestOption . Last

unManifestOption :: ManifestOption -> Maybe FilePath
unManifestOption (ManifestOption x) = getLast x

class HasManifestOption env where
  manifestOptionL :: Lens' env ManifestOption
