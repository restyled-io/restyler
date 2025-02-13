-- |
--
-- Module      : Restyler.Config.NoPull
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.NoPull
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
    <$> withDefault
      True
      ( yesNoSwitch
          [ help "Explicitly pull images before running them"
          , name "pull"
          ]
      )
