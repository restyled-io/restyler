-- |
--
-- Module      : Restyler.Options.NoClean
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.NoClean
  ( HasOption
  , NoClean
  , noCleanSpec
  , getNoClean
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative qualified as Opt
import Restyler.Option

data NoClean

noCleanSpec :: OptionSpec NoClean Bool
noCleanSpec =
  OptionSpec
    { envParser = Env.flag Nothing (Just True) "NO_CLEAN" $ Env.help help
    , optParser =
        Opt.flag Nothing (Just True)
          $ mconcat
            [ Opt.long "no-clean"
            , Opt.help help
            ]
    }
 where
  help :: String
  help = "Don't run git-clean after restyling"

getNoClean :: (MonadReader env m, HasOption NoClean env Bool) => m Bool
getNoClean = lookupOptionDefault @NoClean False
