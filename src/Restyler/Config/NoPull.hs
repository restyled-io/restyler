-- |
--
-- Module      : Restyler.Options.NoPull
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.NoPull
  ( HasNoPull (..)
  , noPullParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasNoPull env where
  getNoPull :: env -> Bool

noPullParser :: Parser Bool
noPullParser =
  not
    <$> yesNoSwitch
      [ help "docker-pull images before docker-run"
      , long "pull"
      , env "PULL"
      , conf "pull"
      ]
