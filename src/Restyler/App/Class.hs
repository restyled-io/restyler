module Restyler.App.Class
  ( MonadSystem (..)
  , MonadDownloadFile (..)
  , readFile
  ) where

import Restyler.Prelude

class Monad m => MonadSystem m where
  getCurrentDirectory :: m FilePath
  setCurrentDirectory :: FilePath -> m ()
  doesFileExist :: FilePath -> m Bool
  doesDirectoryExist :: FilePath -> m Bool
  isFileExecutable :: FilePath -> m Bool
  isFileSymbolicLink :: FilePath -> m Bool
  listDirectory :: FilePath -> m [FilePath]
  readFileBS :: FilePath -> m ByteString
  writeFile :: FilePath -> Text -> m ()
  removeFile :: FilePath -> m ()

readFile :: MonadSystem m => FilePath -> m Text
readFile = fmap (decodeUtf8With lenientDecode) . readFileBS

class Monad m => MonadDownloadFile m where
  downloadFile :: URL -> FilePath -> m ()
