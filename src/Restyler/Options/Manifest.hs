-- |
--
-- Module      : Restyler.Options.Manifest
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.Manifest
  ( ManifestOption (..)
  , HasManifestOption (..)
  , getManifest
  , envManifestOption
  , optManifestOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative

newtype ManifestOption = ManifestOption
  { unwrap :: Last FilePath
  }
  deriving newtype (Semigroup, Monoid)

class HasManifestOption a where
  getManifestOption :: a -> ManifestOption

getManifest :: (MonadReader env m, HasManifestOption env) => m (Maybe FilePath)
getManifest = asks $ getLast . (.unwrap) . getManifestOption

envManifestOption :: Env.Parser Env.Error ManifestOption
envManifestOption =
  ManifestOption
    . Last
    <$> optional (Env.var Env.nonempty "MANIFEST" (Env.help optionHelp))

optManifestOption :: Parser ManifestOption
optManifestOption =
  ManifestOption
    . Last
    <$> optional
      ( option
          str
          ( long "manifest"
              <> metavar "FILE"
              <> help optionHelp
          )
      )

optionHelp :: String
optionHelp = "Restylers manifest to use"
