-- |
--
-- Module      : Restyler.Config.Enabled
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Enabled
  ( HasEnabled (..)
  , enabledParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasEnabled env where
  getEnabled :: env -> Bool

enabledParser :: Parser Bool
enabledParser =
  setting
    [ help "Do anything at all"
    , conf "enabled"
    ]
