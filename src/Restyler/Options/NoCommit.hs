-- |
--
-- Module      : Restyler.Options.NoCommit
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.NoCommit
  ( HasOption
  , NoCommit
  , noCommitSpec
  , getNoCommit
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative qualified as Opt
import Restyler.Option

data NoCommit

noCommitSpec :: OptionSpec NoCommit Bool
noCommitSpec =
  OptionSpec
    { envParser = Env.flag Nothing (Just True) "NO_COMMIT" $ Env.help help
    , optParser =
        Opt.flag Nothing (Just True)
          $ mconcat
            [ Opt.long "no-commit"
            , Opt.help help
            ]
    }
 where
  help :: String
  help = "Don't make commits for restyle changes"

getNoCommit :: (MonadReader env m, HasOption NoCommit env Bool) => m Bool
getNoCommit = lookupOptionDefault @NoCommit False
