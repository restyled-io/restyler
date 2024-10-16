-- |
--
-- Module      : Restyler.Options.ImageCleanup
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.ImageCleanup
  ( HasOption
  , ImageCleanup
  , imageCleanupSpec
  , getImageCleanup
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative qualified as Opt
import Restyler.Option

data ImageCleanup

imageCleanupSpec :: OptionSpec ImageCleanup Bool
imageCleanupSpec =
  OptionSpec
    { envParser = Env.flag Nothing (Just True) "IMAGE_CLEANUP" $ Env.help help
    , optParser =
        Opt.flag Nothing (Just True)
          $ mconcat
            [ Opt.long "image-cleanup"
            , Opt.help help
            ]
    }
 where
  help :: String
  help = "Remove pulled restyler images after restyling"

getImageCleanup
  :: (MonadReader env m, HasOption ImageCleanup env Bool) => m Bool
getImageCleanup = lookupOptionDefault @ImageCleanup False
