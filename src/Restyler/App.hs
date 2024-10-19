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

{- FOURMOLU_DISABLE -}
instance HasCommitTemplate App where getCommitTemplate = (.config.commitTemplate)
instance HasDryRun App where getDryRun = (.config.dryRun)
instance HasEnabled App where getEnabled = (.config.enabled)
instance HasExclude App where getExclude = (.config.exclude)
instance HasFailOnDifferences App where getFailOnDifferences = (.config.failOnDifferences)
instance HasHostDirectory App where getHostDirectory = (.config.hostDirectory)
instance HasIgnores App where getIgnores = (.config.ignores)
instance HasImageCleanup App where getImageCleanup = (.config.imageCleanup)
instance HasManifest App where getManifest = (.config.restylersManifest)
instance HasNoClean App where getNoClean = (.config.noClean)
instance HasNoCommit App where getNoCommit = (.config.noCommit)
instance HasNoPull App where getNoPull = (.config.noPull)
instance HasRemoteFiles App where getRemoteFiles = (.config.remoteFiles)
instance HasRestrictions App where getRestrictions = (.config.restrictions)
instance HasRestylerOverrides App where getRestylerOverrides = (.config.restylerOverrides)
instance HasRestylersVersion App where getRestylersVersion = (.config.restylersVersion)
{- FOURMOLU_ENABLE -}

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

withApp :: (App -> IO a) -> IO a
withApp f = do
  config <- parseConfig
  logSettings <- config.logSettings <$> LogSettings.parse
  withLogger logSettings $ f . App config
