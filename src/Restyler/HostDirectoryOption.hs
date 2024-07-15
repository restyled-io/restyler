module Restyler.HostDirectoryOption
  ( HostDirectoryOption (..)
  , HasHostDirectoryOption (..)
  , toHostDirectoryOption
  , unHostDirectoryOption
  , envHostDirectoryOption
  , optHostDirectoryOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative

newtype HostDirectoryOption = HostDirectoryOption (Last FilePath)
  deriving newtype (Semigroup, Monoid)

class HasHostDirectoryOption env where
  hostDirectoryOptionL :: Lens' env HostDirectoryOption

toHostDirectoryOption :: Maybe FilePath -> HostDirectoryOption
toHostDirectoryOption = HostDirectoryOption . Last

unHostDirectoryOption :: HostDirectoryOption -> Maybe FilePath
unHostDirectoryOption (HostDirectoryOption x) = getLast x

envHostDirectoryOption :: Env.Parser Env.Error HostDirectoryOption
envHostDirectoryOption =
  toHostDirectoryOption
    <$> optional (Env.var Env.nonempty "HOST_DIRECTORY" $ Env.help optionHelp)

optHostDirectoryOption :: Parser HostDirectoryOption
optHostDirectoryOption =
  toHostDirectoryOption
    <$> optional
      (option str $ long "host-directory" <> metavar "DIRECTORY" <> help optionHelp)

optionHelp :: String
optionHelp = "Working directory on host, if dockerized"
