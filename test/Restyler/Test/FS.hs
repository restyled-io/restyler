{-# LANGUAGE UndecidableInstances #-}

-- | Naive implementation of an in-memory filesystem
--
-- Limitations:
--
-- - We try to store absolute paths and normalize function arguments as relative
--   to the mocked current working directory. However, we don't do anything
--   special with . or .. arguments.
--
-- - Directory support is limited. They work a bit like S3: they're just a
--   common prefix shared by multiple files. An empty directory can be created
--   by creating a file ending in @/@. Therefore, reading and writing to a
--   \"directory\" could behave in surprising ways.
--
-- Module      : Restyler.Test.FS
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Test.FS
  ( FS
  , build

    -- * Deriving via for 'MonadDirectory', 'MonadReadFile', 'MonadWriteFile'
  , HasFS (..)
  , ReaderFS (..)
  ) where

import Restyler.Prelude

import Data.List.Extra (dropPrefix)
import Data.Map.Strict qualified as Map
import Restyler.Monad.Directory
import Restyler.Monad.ReadFile
import Restyler.Monad.WriteFile
import System.Directory qualified as Directory
import System.FilePath (addTrailingPathSeparator, isAbsolute, (</>))

data FS' = FS'
  { cwd :: FilePath
  , files :: Map FilePath ReadableFile
  }

data ReadableFile
  = ReadableFile (Text, Directory.Permissions)
  | Symlink FilePath

normalFile :: Text -> ReadableFile
normalFile x =
  ReadableFile
    ( x
    , Directory.emptyPermissions
        { Directory.readable = True
        , Directory.writable = True
        , Directory.executable = False
        , Directory.searchable = True
        }
    )

newtype FS = FS {unwrap :: IORef FS'}

build :: MonadIO m => FilePath -> [(FilePath, Text)] -> m FS
build cwd files =
  FS
    <$> newIORef
      FS'
        { cwd
        , files = Map.fromList $ map (second normalFile) files
        }

class HasFS env where
  fsL :: Lens' env FS

instance HasFS FS where
  fsL = id

readFS' :: (HasFS env, MonadIO m, MonadReader env m) => m FS'
readFS' = readIORef . (.unwrap) =<< view fsL

modifyFS' :: (HasFS env, MonadIO m, MonadReader env m) => (FS' -> FS') -> m ()
modifyFS' f = do
  FS ref <- view fsL
  liftIO $ atomicModifyIORef' ref $ \fs -> (f fs, ())

modifyFiles
  :: (HasFS env, MonadIO m, MonadReader env m)
  => (Map FilePath ReadableFile -> Map FilePath ReadableFile)
  -> m ()
modifyFiles f = modifyFS' $ \fs -> fs {files = f fs.files}

readReadableFile
  :: (HasFS env, MonadIO m, MonadReader env m)
  => FilePath
  -> m (Text, Directory.Permissions)
readReadableFile path' = do
  path <- getAbsolutePath path'
  mContent <- Map.lookup path . (.files) <$> readFS'

  case mContent of
    Nothing -> throwFileNotFound path
    Just (ReadableFile x) -> pure x
    Just (Symlink target) -> readReadableFile target

-- | Naive error for now because the semantics don't matter. We could throw the
-- same error you get from a real read of a missing file.
throwFileNotFound :: FilePath -> a
throwFileNotFound path = error $ pack $ "File does not exist: " <> path

writeReadableFile
  :: (HasFS env, MonadIO m, MonadReader env m)
  => FilePath
  -> ReadableFile
  -> m ()
writeReadableFile path' content = do
  path <- getAbsolutePath path'
  modifyFiles $ Map.insert path content

doesPathExist
  :: (HasFS env, MonadIO m, MonadReader env m) => FilePath -> m Bool
doesPathExist path' = do
  path <- getAbsolutePath path'
  Map.member path . (.files) <$> readFS'

getAbsolutePath
  :: (HasFS env, MonadIO m, MonadReader env m) => FilePath -> m FilePath
getAbsolutePath path
  | isAbsolute path = pure path
  | otherwise = do
      FS' {cwd} <- readFS'
      pure $ cwd </> path

getPrefixed
  :: (HasFS env, MonadIO m, MonadReader env m) => String -> m [FilePath]
getPrefixed prefix = do
  paths <- Map.keys . (.files) <$> readFS'
  pure $ filter (prefix `isPrefixOf`) paths

newtype ReaderFS m a = ReaderFS
  { unwrap :: m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader env
    )

instance (HasFS env, MonadIO m, MonadReader env m) => MonadDirectory (ReaderFS m) where
  getCurrentDirectory = (.cwd) <$> readFS'

  setCurrentDirectory cwd = modifyFS' $ \fs -> fs {cwd}

  doesFileExist path' =
    (\isPath isDirectory -> isPath && not isDirectory)
      <$> doesPathExist path'
      <*> doesDirectoryExist path'

  doesDirectoryExist path' = do
    path <- getAbsolutePath path'
    let prefix = addTrailingPathSeparator path
    not . null <$> getPrefixed prefix

  getPermissions = fmap snd . readReadableFile

  setPermissions path' p = do
    path <- getAbsolutePath path'
    modifyFiles
      $ Map.alter
        ( \case
            Nothing -> throwFileNotFound path
            Just (ReadableFile (x, _)) -> Just $ ReadableFile (x, p)
            Just rf@Symlink {} -> Just rf -- ignore
        )
        path

  createFileLink path' = writeReadableFile path' . Symlink

  pathIsSymbolicLink path' = do
    path <- getAbsolutePath path'
    maybe False check . Map.lookup path . (.files) <$> readFS'
   where
    check = \case
      Symlink _ -> True
      _ -> False

  listDirectory path' = do
    path <- getAbsolutePath path'
    let prefix = addTrailingPathSeparator path
    filter (not . null) . map (dropPrefix prefix) <$> getPrefixed prefix

  removeFile path' = do
    path <- getAbsolutePath path'
    modifyFiles $ Map.delete path

instance (HasFS env, MonadIO m, MonadReader env m) => MonadReadFile (ReaderFS m) where
  readFile = fmap fst . readReadableFile

instance (HasFS env, MonadIO m, MonadReader env m) => MonadWriteFile (ReaderFS m) where
  writeFile path = writeReadableFile path . normalFile
