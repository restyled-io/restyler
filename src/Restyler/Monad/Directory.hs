module Restyler.Monad.Directory
  ( MonadDirectory (..)
  , Permissions (..)
  , isFileExecutable
  , modifyPermissions

    -- * DerivingVia
  , ActualDirectory (..)
  ) where

import Restyler.Prelude

import UnliftIO.Directory (Permissions (..))
import UnliftIO.Directory qualified as Directory

class Monad m => MonadDirectory m where
  getCurrentDirectory :: m FilePath
  setCurrentDirectory :: FilePath -> m ()
  doesFileExist :: FilePath -> m Bool
  doesDirectoryExist :: FilePath -> m Bool
  getPermissions :: FilePath -> m Permissions
  setPermissions :: FilePath -> Permissions -> m ()
  createFileLink :: FilePath -> FilePath -> m ()
  pathIsSymbolicLink :: FilePath -> m Bool
  listDirectory :: FilePath -> m [FilePath]
  removeFile :: FilePath -> m ()

newtype ActualDirectory m a = ActualDocker
  { unwrap :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    )

instance (MonadUnliftIO m, MonadLogger m) => MonadDirectory (ActualDirectory m) where
  getCurrentDirectory = do
    logTrace "getCurrentDirectory"
    liftIO Directory.getCurrentDirectory

  setCurrentDirectory path = do
    logTrace $ "setCurrentDirectory" :# ["path" .= path]
    liftIO $ Directory.setCurrentDirectory path

  doesFileExist path = do
    logTrace $ "doesFileExist" :# ["path" .= path]
    liftIO $ Directory.doesFileExist path

  doesDirectoryExist path = do
    logTrace $ "doesDirectoryExist" :# ["path" .= path]
    liftIO $ Directory.doesDirectoryExist path

  getPermissions path = do
    logTrace $ "getPermissions" :# ["path" .= path]
    liftIO $ Directory.getPermissions path

  setPermissions path p = do
    logTrace $ "setPermissions" :# ["path" .= path]
    liftIO $ Directory.setPermissions path p

  createFileLink path t = do
    logTrace $ "createFileLink" :# ["path" .= path]
    liftIO $ Directory.createFileLink path t

  pathIsSymbolicLink path = do
    logTrace $ "pathIsSymbolicLink" :# ["path" .= path]
    liftIO $ Directory.pathIsSymbolicLink path

  listDirectory path = do
    logTrace $ "listDirectory" :# ["path" .= path]
    liftIO $ Directory.listDirectory path

  removeFile path = do
    logTrace $ "removeFile" :# ["path" .= path]
    liftIO $ Directory.removeFile path

isFileExecutable :: MonadDirectory m => FilePath -> m Bool
isFileExecutable = fmap Directory.executable . getPermissions

modifyPermissions
  :: MonadDirectory m => FilePath -> (Permissions -> Permissions) -> m ()
modifyPermissions path f = do
  p <- getPermissions path
  setPermissions path $ f p
