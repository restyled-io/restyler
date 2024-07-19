{-# LANGUAGE DerivingVia #-}

module Restyler.App
  ( AppT
  , runAppT
  ) where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Network.HTTP.Simple hiding (Request)
import Relude qualified as Prelude
import Restyler.App.Class
import Restyler.Docker
import Restyler.Git
import Restyler.GitHub.Api
import System.Directory qualified as Directory

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
  deriving
    (MonadLogger, MonadLoggerIO)
    via (WithLogger app m)

instance (MonadUnliftIO m, HasLogger app) => MonadSystem (AppT app m) where
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

  isFileExecutable path = do
    logTrace $ "isFileExecutable" :# ["path" .= path]
    liftIO $ Directory.executable <$> Directory.getPermissions path

  isFileSymbolicLink path = do
    logTrace $ "isFileSymbolicLink" :# ["path" .= path]
    liftIO $ Directory.pathIsSymbolicLink path

  listDirectory path = do
    logTrace $ "listDirectory" :# ["path" .= path]
    liftIO $ Directory.listDirectory path

  readFileBS path = do
    logTrace $ "readFileBS" :# ["path" .= path]
    liftIO $ Prelude.readFileBS path

  writeFile path content = do
    logTrace $ "writeFile" :# ["path" .= path]
    liftIO $ Prelude.writeFile path $ unpack content

  removeFile path = do
    logTrace $ "removeFile" :# ["path" .= path]
    liftIO $ Directory.removeFile path

instance (MonadUnliftIO m, HasLogger app) => MonadDownloadFile (AppT app m) where
  downloadFile url path = do
    logDebug $ "downloadFile" :# ["url" .= url]
    liftIO $ do
      request <- parseRequestThrow $ unpack $ getUrl url
      runResourceT $ httpSink request $ \_ -> sinkFile path

deriving via
  (ActualGitHub (AppT app m))
  instance
    (MonadUnliftIO m, HasLogger app, HasGitHubToken app) => MonadGitHub (AppT app m)

deriving via
  (ActualGit (AppT app m))
  instance
    (MonadUnliftIO m, HasLogger app) => MonadGit (AppT app m)

deriving via
  (ActualDocker (AppT app m))
  instance
    (MonadUnliftIO m, HasLogger app) => MonadDocker (AppT app m)

runAppT :: HasCallStack => app -> (HasCallStack => AppT app m a) -> m a
runAppT app f = runReaderT f.unwrap app
