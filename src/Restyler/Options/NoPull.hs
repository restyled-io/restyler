-- |
--
-- Module      : Restyler.Options.NoPull
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.NoPull
  ( HasOption
  , NoPull
  , noPullSpec
  , getNoPull
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative qualified as Opt
import Restyler.Option

data NoPull

noPullSpec :: OptionSpec NoPull Bool
noPullSpec =
  OptionSpec
    { envParser = Env.flag Nothing (Just True) "NO_PULL" $ Env.help help
    , optParser =
        Opt.flag Nothing (Just True)
          $ mconcat
            [ Opt.long "no-pull"
            , Opt.help help
            ]
    }
 where
  help :: String
  help = "Don't docker-pull images before docker-run"

getNoPull :: (MonadReader env m, HasOption NoPull env Bool) => m Bool
getNoPull = lookupOptionDefault @NoPull False
