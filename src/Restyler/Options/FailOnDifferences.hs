-- |
--
-- Module      : Restyler.Options.FailOnDifferences
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.FailOnDifferences
  ( HasOption
  , FailOnDifferences
  , failOnDifferencesSpec
  , getFailOnDifferences
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative qualified as Opt
import Restyler.Option

data FailOnDifferences

failOnDifferencesSpec :: OptionSpec FailOnDifferences Bool
failOnDifferencesSpec =
  OptionSpec
    { envParser = Env.flag Nothing (Just True) "FAIL_ON_DIFFERENCES" $ Env.help help
    , optParser =
        Opt.flag Nothing (Just True)
          $ mconcat
            [ Opt.long "fail-on-differences"
            , Opt.help help
            ]
    }
 where
  help :: String
  help = "Exit non-zero if differences were found"

getFailOnDifferences
  :: (MonadReader env m, HasOption FailOnDifferences env Bool) => m Bool
getFailOnDifferences = lookupOptionDefault @FailOnDifferences False
