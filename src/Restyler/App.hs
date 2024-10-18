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

import Blammo.Logging.LogSettings.Env qualified as LogSettings
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Restyler.Config
import Restyler.Monad.Directory
import Restyler.Monad.Docker
import Restyler.Monad.DownloadFile
import Restyler.Monad.Git
import Restyler.Monad.ReadFile
import Restyler.Monad.WriteFile
import Restyler.Options

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
  { config :: Config
  , logger :: Logger
  }
  deriving
    ( HasCommitTemplate
    , HasIgnores
    , HasExclude
    , HasRemoteFiles
    , HasRestylersVersion
    , HasRestylerOverrides
    )
    via (ThroughConfig App)
  deriving
    ( HasDryRun
    , HasFailOnDifferences
    , HasHostDirectory
    , HasImageCleanup
    , HasManifest
    , HasNoClean
    , HasNoCommit
    , HasNoPull
    , HasRestrictions
    )
    via (ThroughOptions App)

instance HasConfig App where
  getConfig = (.config)

instance HasOptions App where
  getOptions = (.config.options)

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

withApp :: (App -> IO a) -> IO a
withApp f = do
  config <- parseConfig
  logSettings <- config.options.logSettings <$> LogSettings.parse
  withLogger logSettings $ f . App config
