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
module Restyler.Test.FS
    ( HasFS(..)
    , FS
    , build
    , readFileUtf8
    , readFileBinary
    , writeFileUtf8
    , writeFileExecutable
    , createFileLink
    , getCurrentDirectory
    , setCurrentDirectory
    -- , doesPathExist
    , doesFileExist
    , doesDirectoryExist
    , isFileExecutable
    , isFileSymbolicLink
    , listDirectory
    ) where

import Restyler.Prelude

import Data.List.Extra (dropPrefix)
import qualified Data.Map.Strict as Map
import qualified System.Directory as Directory
import System.FilePath (addTrailingPathSeparator, isAbsolute, (</>))

class HasFS env where
    fsL :: Lens' env FS

newtype FS = FS { unFS :: IORef FS' }

readFS' :: (MonadIO m, MonadReader env m, HasFS env) => m FS'
readFS' = readIORef . unFS =<< view fsL

modifyFS' :: (MonadIO m, MonadReader env m, HasFS env) => (FS' -> FS') -> m ()
modifyFS' f = do
    FS ref <- view fsL
    liftIO $ atomicModifyIORef' ref $ \fs -> (f fs, ())

data FS' = FS'
    { fsCwd :: FilePath
    , fsFiles :: Map FilePath ReadableFile
    }

data ReadableFile
    = ReadableFile (Text, Directory.Permissions)
    | Symlink FilePath

normalFile :: Text -> ReadableFile
normalFile x = ReadableFile
    ( x
    , Directory.emptyPermissions
        { Directory.readable = True
        , Directory.writable = True
        , Directory.executable = False
        , Directory.searchable = True
        }
    )

executableFile :: Text -> ReadableFile
executableFile x = ReadableFile
    ( x
    , Directory.emptyPermissions
        { Directory.readable = True
        , Directory.writable = True
        , Directory.executable = True
        , Directory.searchable = True
        }
    )

build :: MonadIO m => FilePath -> [(FilePath, Text)] -> m FS
build cwd files = FS <$> newIORef FS'
    { fsCwd = cwd
    , fsFiles = Map.fromList $ map (second normalFile) files
    }

readFileUtf8 :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> m Text
readFileUtf8 = fmap fst . readFile

readFile
    :: (MonadIO m, MonadReader env m, HasFS env)
    => FilePath
    -> m (Text, Directory.Permissions)
readFile path' = do
    path <- getAbsolutePath path'
    mContent <- Map.lookup path . fsFiles <$> readFS'

    case mContent of
        -- We could throw the same error you get from a real read of a missing
        -- file.
        Nothing -> error $ pack $ "File does not exist: " <> path
        Just (ReadableFile x) -> pure x
        Just (Symlink target) -> readFile target

readFileBinary
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> m ByteString
readFileBinary = fmap encodeUtf8 . readFileUtf8

writeFileUtf8
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> Text -> m ()
writeFileUtf8 path = writeFile path . normalFile

writeFileExecutable
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> Text -> m ()
writeFileExecutable path = writeFile path . executableFile

createFileLink
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> FilePath -> m ()
createFileLink target name = writeFile name $ Symlink target

writeFile
    :: (MonadIO m, MonadReader env m, HasFS env)
    => FilePath
    -> ReadableFile
    -> m ()
writeFile path' content = do
    path <- getAbsolutePath path'
    modifyFS' $ \fs -> fs { fsFiles = Map.insert path content $ fsFiles fs }

getCurrentDirectory :: (MonadIO m, MonadReader env m, HasFS env) => m FilePath
getCurrentDirectory = fsCwd <$> readFS'

setCurrentDirectory
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> m ()
setCurrentDirectory cwd = modifyFS' $ \fs -> fs { fsCwd = cwd }

doesPathExist
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> m Bool
doesPathExist path' = do
    path <- getAbsolutePath path'
    Map.member path . fsFiles <$> readFS'

doesFileExist
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> m Bool
doesFileExist path' =
    (\isPath isDirectory -> isPath && not isDirectory)
        <$> doesPathExist path'
        <*> doesDirectoryExist path'

doesDirectoryExist
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> m Bool
doesDirectoryExist path' = do
    path <- getAbsolutePath path'
    let prefix = addTrailingPathSeparator path
    not . null <$> getPrefixed prefix

isFileExecutable
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> m Bool
isFileExecutable = fmap (Directory.executable . snd) . readFile

isFileSymbolicLink
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> m Bool
isFileSymbolicLink path' = do
    path <- getAbsolutePath path'
    maybe False check . Map.lookup path . fsFiles <$> readFS'
  where
    check = \case
        Symlink _ -> True
        _ -> False

listDirectory
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> m [FilePath]
listDirectory path' = do
    path <- getAbsolutePath path'
    let prefix = addTrailingPathSeparator path
    filter (not . null) . map (dropPrefix prefix) <$> getPrefixed prefix

getAbsolutePath
    :: (MonadIO m, MonadReader env m, HasFS env) => FilePath -> m FilePath
getAbsolutePath path
    | isAbsolute path = pure path
    | otherwise = do
        FS' {..} <- readFS'
        pure $ fsCwd </> path

getPrefixed
    :: (MonadIO m, MonadReader env m, HasFS env) => String -> m [FilePath]
getPrefixed prefix = do
    paths <- Map.keys . fsFiles <$> readFS'
    pure $ filter (prefix `isPrefixOf`) paths
