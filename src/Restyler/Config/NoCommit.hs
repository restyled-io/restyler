-- |
--
-- Module      : Restyler.Config.NoCommit
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.NoCommit
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
    <$> withDefault
      False
      ( yesNoSwitch
          [ help "Commit each restyling change"
          , name "commit"
          ]
      )
