-- |
--
-- Module      : Restyler.Options.FailOnDifferences
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.FailOnDifferences
  ( FailOnDifferencesOption (..)
  , HasFailOnDifferencesOption (..)
  , getFailOnDifferences
  , envFailOnDifferencesOption
  , optFailOnDifferencesOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative

newtype FailOnDifferencesOption = FailOnDifferencesOption
  { unwrap :: Any
  }
  deriving newtype (Semigroup, Monoid)

class HasFailOnDifferencesOption a where
  getFailOnDifferencesOption :: a -> FailOnDifferencesOption

getFailOnDifferences
  :: (MonadReader env m, HasFailOnDifferencesOption env) => m Bool
getFailOnDifferences = asks $ getAny . (.unwrap) . getFailOnDifferencesOption

envFailOnDifferencesOption :: Env.Parser Env.Error FailOnDifferencesOption
envFailOnDifferencesOption =
  FailOnDifferencesOption
    . Any
    <$> Env.switch "FAIL_ON_DIFFERENCES" (Env.help optionHelp)

optFailOnDifferencesOption :: Parser FailOnDifferencesOption
optFailOnDifferencesOption =
  FailOnDifferencesOption
    . Any
    <$> switch (long "fail-on-differences" <> help optionHelp)

optionHelp :: String
optionHelp = "Exit non-zero if differences were found"
