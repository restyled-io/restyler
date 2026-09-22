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

-- | Unused, parsed to avoid usage-error
noCleanParser :: Parser ()
noCleanParser =
  void $ (,)
    <$> optional (setting [switch (), long "clean", hidden])
    <*> optional (setting [switch (), long "no-clean", hidden])
