-- |
--
-- Module      : Restyler.Config.KeepGoing
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.KeepGoing
  ( HasKeepGoing (..)
  , keepGoingParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasKeepGoing env where
  getKeepGoing :: env -> Bool

keepGoingParser :: Parser Bool
keepGoingParser =
  withDefault False
    $ yesNoSwitch
      [ help "Keep going if a restyler fails"
      , name "keep-going"
      ]
