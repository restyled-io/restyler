-- |
--
-- Module      : Restyler.Options.DryRun
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.DryRun
  ( HasOption
  , DryRun
  , dryRunSpec
  , getDryRun
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative qualified as Opt
import Restyler.Option

data DryRun

dryRunSpec :: OptionSpec DryRun Bool
dryRunSpec =
  OptionSpec
    { envParser = Env.flag Nothing (Just True) "DRY_RUN" $ Env.help help
    , optParser =
        Opt.flag Nothing (Just True)
          $ mconcat
            [ Opt.long "dry-run"
            , Opt.help help
            ]
    }
 where
  help :: String
  help = "Skip pulling and running Restylers"

getDryRun :: (MonadReader env m, HasOption DryRun env Bool) => m Bool
getDryRun = lookupOptionDefault @DryRun False
