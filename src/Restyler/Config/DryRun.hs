-- |
--
-- Module      : Restyler.Config.DryRun
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.DryRun
  ( HasDryRun (..)
  , dryRunParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasDryRun env where
  getDryRun :: env -> Bool

dryRunParser :: Parser Bool
dryRunParser =
  withDefault False
    $ yesNoSwitch
      [ help "Do everything except pull and run restylers"
      , long "dry-run"
      , env "DRY_RUN"
      , conf "dry_run"
      ]
