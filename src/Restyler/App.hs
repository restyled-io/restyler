{-# LANGUAGE DerivingVia #-}

-- |
--
-- Module      : Restyler.App
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.App
  ( AppT
  , runAppT
  , App (..)
  , withApp
  ) where

import Restyler.Prelude

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Extra (findM)
import Data.Yaml qualified as Yaml
import Env qualified
import Relude qualified
import Restyler.Config (configPaths, formatYamlException)
import Restyler.Monad.Directory
import Restyler.Monad.Docker
import Restyler.Monad.DownloadFile
import Restyler.Monad.Git
import Restyler.Monad.ReadFile
import Restyler.Monad.WriteFile
import Restyler.Opt qualified as Opt
import Restyler.Options
import UnliftIO.Directory qualified as UnliftIO
import UnliftIO.Exception qualified as UnliftIO

newtype AppT app m a = AppT
  { unwrap :: ReaderT app m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO
    , MonadUnliftIO
    , MonadReader app
    )
  deriving (MonadLogger, MonadLoggerIO) via (WithLogger app m)
  deriving (MonadDirectory) via (ActualDirectory (AppT app m))
  deriving (MonadDocker) via (ActualDocker (AppT app m))
  deriving (MonadDownloadFile) via (ActualDownloadFile (AppT app m))
  deriving (MonadGit) via (ActualGit (AppT app m))
  deriving (MonadReadFile) via (ActualReadFile (AppT app m))
  deriving (MonadWriteFile) via (ActualWriteFile (AppT app m))

runAppT :: app -> AppT app m a -> m a
runAppT app f = runReaderT f.unwrap app

data App = App
  { logger :: Logger
  , options :: Options
  }

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasOption t Options a => HasOption t App a where
  getOption = getOption . (.options)

withApp :: (App -> IO a) -> IO a
withApp f = do
  restyledYml <-
    findM UnliftIO.doesFileExist configPaths
      `UnliftIO.catchAny` \_ -> pure Nothing

  -- Load ENV + CLI to override
  cli <-
    (<>)
      <$> Opt.parse "Restyle local files" (Env.helpDoc 80 envParser) optParser
      <*> Env.parse id envParser

  -- Load restyled.yml (or --config)
  config <-
    maybe (pure mempty) decodeConfigThrow $ restyledYml <> getConfigPath cli

  let options = config <> cli

  withLogger (getLogSettings options) $ \logger -> do
    f $ App {logger, options}

decodeConfigThrow :: FromJSON a => FilePath -> IO a
decodeConfigThrow path = do
  bs <- Relude.readFileBS path
  let result = Yaml.decodeEither' bs
  either (die . unpack . formatYamlException path bs) pure result
