-- |
--
-- Module      : Restyler.Options.Manifest
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.Manifest
  ( HasOption
  , Manifest
  , manifestSpec
  , getManifest
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative qualified as Opt
import Restyler.Option

data Manifest

manifestSpec :: OptionSpec Manifest FilePath
manifestSpec =
  OptionSpec
    { envParser = optional $ Env.var Env.nonempty "MANIFEST" $ Env.help help
    , optParser =
        optional
          $ Opt.strOption
          $ mconcat
            [ Opt.long "manifest"
            , Opt.metavar "FILE"
            , Opt.help help
            ]
    }
 where
  help :: String
  help = "Restylers manifest to use"

getManifest
  :: (MonadReader env m, HasOption Manifest env FilePath) => m (Maybe FilePath)
getManifest = lookupOption @Manifest
