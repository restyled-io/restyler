-- |
--
-- Module      : Restyler.Config.FailOnDifferences
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.FailOnDifferences
  ( HasFailOnDifferences (..)
  , failOnDifferencesParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasFailOnDifferences env where
  getFailOnDifferences :: env -> Bool

failOnDifferencesParser :: Parser Bool
failOnDifferencesParser =
  withDefault False
    $ yesNoSwitch
      [ help "Exit non-zero if differences were found"
      , long "fail-on-differences"
      , env "FAIL_ON_DIFFERENCES"
      , conf "fail_on_differences"
      ]
