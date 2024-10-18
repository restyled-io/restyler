-- |
--
-- Module      : Restyler.Config.NoClean
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.NoClean
  ( HasNoClean (..)
  , noCleanParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasNoClean env where
  getNoClean :: env -> Bool

noCleanParser :: Parser Bool
noCleanParser =
  not
    <$> yesNoSwitch
      [ help "Run git-clean after restyling"
      , long "clean"
      , env "CLEAN"
      , conf "clean"
      ]
