-- |
--
-- Module      : Restyler.Options.DryRun
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.DryRun
  ( HasDryRun (..)
  , dryRunParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasDryRun env where
  getDryRun :: env -> Bool

dryRunParser :: Parser Bool
dryRunParser =
  yesNoSwitch
    [ help "Skip pulling and running Restylers"
    , long "dry-run"
    , env "DRY_RUN"
    , conf "dry_run"
    ]
