-- |
--
-- Module      : Restyler.Options.HostDirectory
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.HostDirectory
  ( HasOption
  , HostDirectory
  , hostDirectorySpec
  , getHostDirectory
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative qualified as Opt
import Restyler.Monad.Directory
import Restyler.Option

data HostDirectory

getHostDirectory
  :: ( MonadDirectory m
     , MonadReader env m
     , HasOption HostDirectory env FilePath
     )
  => m FilePath
getHostDirectory = do
  mHostDirectory <- lookupOption @HostDirectory
  maybe getCurrentDirectory pure mHostDirectory

hostDirectorySpec :: OptionSpec HostDirectory FilePath
hostDirectorySpec =
  OptionSpec
    { envParser = optional $ Env.var Env.nonempty "HOST_DIRECTORY" $ Env.help help
    , optParser =
        optional
          $ Opt.strOption
          $ mconcat
            [ Opt.long "host-directory"
            , Opt.metavar "DIRECTORY"
            , Opt.help help
            ]
    }
 where
  help :: String
  help = "Working directory on host, if dockerized"
