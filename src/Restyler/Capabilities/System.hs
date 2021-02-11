module Restyler.Capabilities.System
    ( MonadSystem(..)
    , ActualSystem(..)
    )
where

import Restyler.Prelude

import qualified RIO
import qualified RIO.Directory as Directory

class Monad m => MonadSystem m where
    getCurrentDirectory :: m FilePath
    setCurrentDirectory :: FilePath -> m ()
    doesFileExist :: FilePath -> m Bool
    doesDirectoryExist :: FilePath -> m Bool
    isFileExecutable :: FilePath -> m Bool
    isFileSymbolicLink :: FilePath -> m Bool
    listDirectory :: FilePath -> m [FilePath]
    readFile :: FilePath -> m Text
    readFileBS :: FilePath -> m ByteString
    writeFile :: FilePath -> Text -> m ()

instance MonadSystem m => MonadSystem (ExceptT e m) where
    getCurrentDirectory = lift getCurrentDirectory
    setCurrentDirectory = lift . setCurrentDirectory
    doesFileExist = lift . doesFileExist
    doesDirectoryExist = lift . doesDirectoryExist
    isFileExecutable = lift . isFileExecutable
    isFileSymbolicLink = lift . isFileSymbolicLink
    listDirectory = lift . listDirectory
    readFile = lift . readFile
    readFileBS = lift . readFileBS
    writeFile x = lift . writeFile x

newtype ActualSystem m a = ActualSystem
    { unActualSystem :: m a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadSystem (ActualSystem m) where
    getCurrentDirectory = Directory.getCurrentDirectory
    setCurrentDirectory = Directory.setCurrentDirectory
    doesFileExist = Directory.doesFileExist
    doesDirectoryExist = Directory.doesDirectoryExist
    isFileExecutable = fmap Directory.executable . Directory.getPermissions
    isFileSymbolicLink = Directory.pathIsSymbolicLink
    listDirectory = Directory.listDirectory
    readFile = RIO.readFileUtf8
    readFileBS = RIO.readFileBinary
    writeFile = RIO.writeFileUtf8

-- actualSystem :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => System m
-- actualSystem = System
--     { getCurrentDirectory = do
--         logDebug "getCurrentDirectory"
--         appIO SystemError Directory.getCurrentDirectory
--     , setCurrentDirectory = \path -> do
--         logDebug $ "setCurrentDirectory: " <> displayShow path
--         appIO SystemError $ Directory.setCurrentDirectory path
--     , doesFileExist = \path -> do
--         logDebug $ "doesFileExist: " <> displayShow path
--         appIO SystemError $ Directory.doesFileExist path
--     , doesDirectoryExist = \path -> do
--         logDebug $ "doesDirectoryExist: " <> displayShow path
--         appIO SystemError $ Directory.doesDirectoryExist path
--     , isFileExecutable = \path -> do
--         logDebug $ "isFileExecutable: " <> displayShow path
--         appIO SystemError
--             $ Directory.executable
--             <$> Directory.getPermissions path
--     , isFileSymbolicLink = \path -> do
--         logDebug $ "isFileSymbolicLink: " <> displayShow path
--         appIO SystemError $ Directory.pathIsSymbolicLink path
--     , listDirectory = \path -> do
--         logDebug $ "listDirectory: " <> displayShow path
--         appIO SystemError $ Directory.listDirectory path
--     , readFile = \path -> do
--         logDebug $ "readFile: " <> displayShow path
--         appIO SystemError $ readFileUtf8 path
--     , readFileBS = \path -> do
--         logDebug $ "readFileBS: " <> displayShow path
--         appIO SystemError $ readFileBinary path
--     , writeFile = \path content -> do
--         logDebug $ "writeFile: " <> displayShow path
--         appIO SystemError $ writeFileUtf8 path content
--     }
