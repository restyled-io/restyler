-- |
--
-- Module      : Restyler.Options.NoCommit
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.NoCommit
  ( HasNoCommit (..)
  , noCommitParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasNoCommit env where
  getNoCommit :: env -> Bool

noCommitParser :: Parser Bool
noCommitParser =
  not
    <$> yesNoSwitch
      [ help "Make commits for restyle changes"
      , long "commit"
      , env "COMMIT"
      , conf "commit"
      , value True
      ]
