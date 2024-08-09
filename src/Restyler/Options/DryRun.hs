-- |
--
-- Module      : Restyler.Options.DryRun
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.DryRun
  ( DryRunOption (..)
  , HasDryRunOption (..)
  , getDryRun
  , envDryRunOption
  , optDryRunOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative

newtype DryRunOption = DryRunOption
  { unwrap :: Any
  }
  deriving newtype (Semigroup, Monoid)

class HasDryRunOption a where
  getDryRunOption :: a -> DryRunOption

getDryRun :: (MonadReader env m, HasDryRunOption env) => m Bool
getDryRun = asks $ getAny . (.unwrap) . getDryRunOption

envDryRunOption :: Env.Parser Env.Error DryRunOption
envDryRunOption =
  DryRunOption . Any <$> Env.switch "DRY_RUN" (Env.help optionHelp)

optDryRunOption :: Parser DryRunOption
optDryRunOption =
  DryRunOption . Any <$> switch (long "dry-run" <> help optionHelp)

optionHelp :: String
optionHelp = "Don't docker-pull or docker-run Restylers"
