module Restyler.Options.NoPull
  ( NoPullOption (..)
  , HasNoPullOption (..)
  , getNoPull
  , envNoPullOption
  , optNoPullOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative

newtype NoPullOption = NoPullOption
  { unwrap :: Any
  }
  deriving newtype (Semigroup, Monoid)

class HasNoPullOption a where
  getNoPullOption :: a -> NoPullOption

getNoPull :: (MonadReader env m, HasNoPullOption env) => m Bool
getNoPull = asks $ getAny . (.unwrap) . getNoPullOption

envNoPullOption :: Env.Parser Env.Error NoPullOption
envNoPullOption =
  NoPullOption . Any <$> Env.switch "NO_PULL" (Env.help optionHelp)

optNoPullOption :: Parser NoPullOption
optNoPullOption =
  NoPullOption . Any <$> switch (long "no-pull" <> help optionHelp)

optionHelp :: String
optionHelp = "Don't docker-pull images before docker-run"
