{-# LANGUAGE DerivingVia #-}

module Restyler.App
  ( AppT
  , runAppT
  ) where

import Restyler.Prelude

import Control.Monad.Catch (MonadCatch, MonadThrow)
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
