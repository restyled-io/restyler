-- |
--
-- Module      : Restyler.Options.NoClean
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.NoClean
  ( NoCleanOption (..)
  , HasNoCleanOption (..)
  , getNoClean
  , envNoCleanOption
  , optNoCleanOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative

newtype NoCleanOption = NoCleanOption
  { unwrap :: Any
  }
  deriving newtype (Semigroup, Monoid)

class HasNoCleanOption a where
  getNoCleanOption :: a -> NoCleanOption

getNoClean :: (MonadReader env m, HasNoCleanOption env) => m Bool
getNoClean = asks $ getAny . (.unwrap) . getNoCleanOption

envNoCleanOption :: Env.Parser Env.Error NoCleanOption
envNoCleanOption =
  NoCleanOption . Any <$> Env.switch "NO_CLEAN" (Env.help optionHelp)

optNoCleanOption :: Parser NoCleanOption
optNoCleanOption =
  NoCleanOption . Any <$> switch (long "no-clean" <> help optionHelp)

optionHelp :: String
optionHelp = "Don't run git-clean after restyling"
