module Restyler.Options.HostDirectory
  ( HostDirectoryOption (..)
  , HasHostDirectoryOption (..)
  , getHostDirectory
  , toHostDirectoryOption
  , envHostDirectoryOption
  , optHostDirectoryOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative
import Restyler.App.Class (MonadSystem (..))

newtype HostDirectoryOption = HostDirectoryOption (Last FilePath)
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
  mHostDirectory <- asks $ unHostDirectoryOption . getHostDirectoryOption
  maybe getCurrentDirectory pure mHostDirectory

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
