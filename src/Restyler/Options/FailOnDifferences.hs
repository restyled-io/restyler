-- |
--
-- Module      : Restyler.Options.FailOnDifferences
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.FailOnDifferences
  ( HasFailOnDifferences (..)
  , failOnDifferencesParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasFailOnDifferences env where
  getFailOnDifferences :: env -> Bool

failOnDifferencesParser :: Parser Bool
failOnDifferencesParser =
  yesNoSwitch
    [ help "Exit non-zero if differences were found"
    , long "fail-on-differences"
    , env "FAIL_ON_DIFFERENCES"
    , conf "fail_on_differences"
    ]
