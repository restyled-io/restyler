-- |
--
-- Module      : Restyler.Monad.Directory
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Monad.Directory
  ( MonadDirectory (..)
  , Permissions (..)
  , isFileExecutable
  , modifyPermissions

    -- * DerivingVia
  , ActualDirectory (..)
  ) where

import Restyler.Prelude

import Path.IO qualified as PathIO
import UnliftIO.Directory (Permissions (..))
import UnliftIO.Directory qualified as Directory

class Monad m => MonadDirectory m where
  getCurrentDirectory :: m (Path Abs Dir)
  setCurrentDirectory :: Path Abs Dir -> m ()
  doesFileExist :: forall b. Path b File -> m Bool
  doesDirectoryExist :: forall b. Path b Dir -> m Bool
  getPermissions :: forall b. Path b File -> m Permissions
  setPermissions :: forall b. Path b File -> Permissions -> m ()
  createFileLink :: forall b0 b1. Path b0 File -> Path b1 File -> m ()
  pathIsSymbolicLink :: forall b. Path b File -> m Bool
  listDirectoryRecur :: forall b. Path b Dir -> m [Path Rel File]
  removeFile :: forall b. Path b File -> m ()

newtype ActualDirectory m a = ActualDirectory
  { unwrap :: m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadLogger
    , MonadUnliftIO
    )

instance (MonadLogger m, MonadUnliftIO m) => MonadDirectory (ActualDirectory m) where
  getCurrentDirectory = do
    logTrace "getCurrentDirectory"
    liftIO PathIO.getCurrentDir

  setCurrentDirectory path = do
    logTrace $ "setCurrentDirectory" :# ["path" .= path]
    liftIO $ PathIO.setCurrentDir path

  doesFileExist path = do
    logTrace $ "doesFileExist" :# ["path" .= path]
    liftIO $ PathIO.doesFileExist path

  doesDirectoryExist path = do
    logTrace $ "doesDirectoryExist" :# ["path" .= path]
    liftIO $ PathIO.doesDirExist path

  getPermissions path = do
    logTrace $ "getPermissions" :# ["path" .= path]
    liftIO $ PathIO.getPermissions path

  setPermissions path p = do
    logTrace $ "setPermissions" :# ["path" .= path]
    liftIO $ PathIO.setPermissions path p

  createFileLink path t = do
    logTrace $ "createFileLink" :# ["path" .= path]
    liftIO $ PathIO.createFileLink path t

  pathIsSymbolicLink path = do
    logTrace $ "pathIsSymbolicLink" :# ["path" .= path]
    liftIO $ PathIO.isSymlink path

  listDirectoryRecur path = do
    logTrace $ "listDirectoryRecur" :# ["path" .= path]
    liftIO $ snd <$> PathIO.listDirRecurRel path

  removeFile path = do
    logTrace $ "removeFile" :# ["path" .= path]
    liftIO $ PathIO.removeFile path

isFileExecutable :: MonadDirectory m => Path b File -> m Bool
isFileExecutable = fmap Directory.executable . getPermissions

modifyPermissions
  :: MonadDirectory m => Path b File -> (Permissions -> Permissions) -> m ()
modifyPermissions path f = do
  p <- getPermissions path
  setPermissions path $ f p
