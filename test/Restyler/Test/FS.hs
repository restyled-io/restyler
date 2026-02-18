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

import Data.Map.Strict qualified as Map
import Path
import Path.IO qualified as Directory
import Restyler.Monad.Directory
import Restyler.Monad.ReadFile
import Restyler.Monad.WriteFile

data FS' = FS'
  { cwd :: Path Abs Dir
  , files :: Map (Path Abs File) ReadableFile
  }
  deriving stock (Show)

data ReadableFile
  = ReadableFile (Text, Directory.Permissions)
  | Symlink (Path Abs File)
  deriving stock (Show)

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

build :: MonadIO m => Path Abs Dir -> [(Path Abs File, Text)] -> m FS
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
  => (Map (Path Abs File) ReadableFile -> Map (Path Abs File) ReadableFile)
  -> m ()
modifyFiles f = modifyFS' $ \fs -> fs {files = f fs.files}

readReadableFile
  :: (HasFS env, MonadIO m, MonadReader env m)
  => Path b File
  -> m (Text, Directory.Permissions)
readReadableFile path' = do
  path <- makeAbsoluteFile path'
  mContent <- Map.lookup path . (.files) <$> readFS'

  case mContent of
    Nothing -> throwFileNotFound path
    Just (ReadableFile x) -> pure x
    Just (Symlink target) -> readReadableFile target

-- | Naive error for now because the semantics don't matter. We could throw the
-- same error you get from a real read of a missing file.
throwFileNotFound :: Path b t -> a
throwFileNotFound path = error $ pack $ "File does not exist: " <> toFilePath path

writeReadableFile
  :: (HasFS env, MonadIO m, MonadReader env m)
  => Path b File
  -> ReadableFile
  -> m ()
writeReadableFile path' content = do
  path <- makeAbsoluteFile path'
  modifyFiles $ Map.insert path content

makeAbsolute
  :: ( Directory.AnyPath path
     , Directory.RelPath path ~ Path Rel t
     , HasFS env
     , MonadIO m
     , MonadReader env m
     )
  => path
  -> m (Path Abs t)
makeAbsolute path = do
  FS' {cwd} <- readFS'
  rel <- liftIO $ Directory.makeRelative cwd path
  pure $ cwd </> rel

makeAbsoluteDir
  :: (HasFS env, MonadIO m, MonadReader env m) => Path b Dir -> m (Path Abs Dir)
makeAbsoluteDir = makeAbsolute

makeAbsoluteFile
  :: (HasFS env, MonadIO m, MonadReader env m) => Path b File -> m (Path Abs File)
makeAbsoluteFile = makeAbsolute

getPrefixed
  :: (HasFS env, MonadIO m, MonadReader env m) => Path Abs Dir -> m [Path Abs File]
getPrefixed prefix = do
  paths <- Map.keys . (.files) <$> readFS'
  pure $ filter (prefix `isProperPrefixOf`) paths

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

  doesFileExist path' = do
    path <- makeAbsoluteFile path'
    Map.member path . (.files) <$> readFS'

  doesDirectoryExist path' = do
    path <- makeAbsoluteDir path'
    not . null <$> getPrefixed path

  getPermissions = fmap snd . readReadableFile

  setPermissions path' p = do
    path <- makeAbsoluteFile path'
    modifyFiles
      $ Map.alter
        ( \case
            Nothing -> throwFileNotFound path
            Just (ReadableFile (x, _)) -> Just $ ReadableFile (x, p)
            Just rf@Symlink {} -> Just rf -- ignore
        )
        path

  createFileLink target' path' = do
    target <- makeAbsoluteFile target'
    path <- makeAbsoluteFile path'
    writeReadableFile path $ Symlink target

  pathIsSymbolicLink path' = do
    path <- makeAbsoluteFile path'
    maybe False check . Map.lookup path . (.files) <$> readFS'
   where
    check = \case
      Symlink _ -> True
      _ -> False

  listDirectoryRecur path' = do
    path <- makeAbsoluteDir path'
    paths <- Map.keys . (.files) <$> readFS'
    pure $ mapMaybe (stripProperPrefix path) paths

  removeFile path' = do
    path <- makeAbsoluteFile path'
    modifyFiles $ Map.delete path

instance (HasFS env, MonadIO m, MonadReader env m) => MonadReadFile (ReaderFS m) where
  readFile = fmap fst . readReadableFile

instance (HasFS env, MonadIO m, MonadReader env m) => MonadWriteFile (ReaderFS m) where
  writeFile path = writeReadableFile path . normalFile
