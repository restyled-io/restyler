module Restyler.Options.FailOnDifferences
  ( FailOnDifferencesOption (..)
  , HasFailOnDifferencesOption (..)
  , getFailOnDifferences
  , envFailOnDifferences
  , optFailOnDifferences
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

envFailOnDifferences :: Env.Parser Env.Error FailOnDifferencesOption
envFailOnDifferences =
  FailOnDifferencesOption
    . Any
    <$> Env.switch "FAIL_ON_DIFFERENCES" (Env.help optionHelp)

optFailOnDifferences :: Parser FailOnDifferencesOption
optFailOnDifferences =
  FailOnDifferencesOption
    . Any
    <$> switch (long "fail-on-differences" <> help optionHelp)

optionHelp :: String
optionHelp = "Exit non-zero if differences were found"
