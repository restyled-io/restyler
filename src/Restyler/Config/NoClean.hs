-- |
--
-- Module      : Restyler.Config.NoClean
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.NoClean
  ( noCleanParser
  ) where

import Restyler.Prelude

import OptEnvConf

noCleanParser :: Parser Bool
noCleanParser =
  not
    <$> withDefault
      False
      ( yesNoSwitch
          [ help "Unused"
          , name "clean"
          , hidden
          ]
      )
