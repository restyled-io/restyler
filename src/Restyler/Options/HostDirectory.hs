module Restyler.Options.HostDirectory
  ( HostDirectoryOption (..)
  , HasHostDirectoryOption (..)
  , getHostDirectory
  , envHostDirectoryOption
  , optHostDirectoryOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative
import Restyler.App.Class (MonadSystem (..))

newtype HostDirectoryOption = HostDirectoryOption
  { unwrap :: Last FilePath
  }
  deriving newtype (Semigroup, Monoid)

class HasHostDirectoryOption env where
  getHostDirectoryOption :: env -> HostDirectoryOption

getHostDirectory
  :: ( MonadSystem m
     , MonadReader env m
     , HasHostDirectoryOption env
     )
  => m FilePath
getHostDirectory = do
  mHostDirectory <- asks $ getLast . (.unwrap) . getHostDirectoryOption
  maybe getCurrentDirectory pure mHostDirectory

envHostDirectoryOption :: Env.Parser Env.Error HostDirectoryOption
envHostDirectoryOption =
  HostDirectoryOption
    . Last
    <$> optional (Env.var Env.nonempty "HOST_DIRECTORY" $ Env.help optionHelp)

optHostDirectoryOption :: Parser HostDirectoryOption
optHostDirectoryOption =
  HostDirectoryOption
    . Last
    <$> optional
      (option str $ long "host-directory" <> metavar "DIRECTORY" <> help optionHelp)

optionHelp :: String
optionHelp = "Working directory on host, if dockerized"
