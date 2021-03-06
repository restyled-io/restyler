-- | Naive implementation of an in-memory filesystem
--
-- This aims to be generic to the @"RIO"@ FS-related functions, but is of course
-- only written to implement our own @'HasSystem'@ class so far.
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
module RIO.Test.FS
    ( HasFS(..)
    , FS
    , build
    , readFileUtf8
    , readFileBinary
    , writeFileUtf8
    , writeFileExecutable
    , writeFileUnreadable
    , createFileLink
    , getCurrentDirectory
    , setCurrentDirectory
    -- , doesPathExist
    , doesFileExist
    , doesDirectoryExist
    , isFileExecutable
    , isFileSymbolicLink
    , listDirectory
    )
where

import RIO hiding (readFileBinary, readFileUtf8, writeFileUtf8)

import qualified Data.Map.Strict as Map
import RIO.FilePath (addTrailingPathSeparator, isAbsolute, (</>))
import RIO.List (dropPrefix, isPrefixOf)
import qualified System.Directory as Directory

class HasFS env where
    fsL :: Lens' env FS

newtype FS = FS { unFS :: IORef FS' }

readFS' :: HasFS env => RIO env FS'
readFS' = readIORef . unFS =<< view fsL

modifyFS' :: HasFS env => (FS' -> FS') -> RIO env ()
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
    | UnreadableFile IOException

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

readFileUtf8 :: HasFS env => FilePath -> RIO env Text
readFileUtf8 = fmap fst . readFile

readFile :: HasFS env => FilePath -> RIO env (Text, Directory.Permissions)
readFile path' = do
    path <- getAbsolutePath path'
    mContent <- Map.lookup path . fsFiles <$> readFS'

    case mContent of
        -- We could throw the same error you get from a real read of a missing
        -- file. However, you should be intentional about testing such a
        -- scenario and use writeFileUnreadable to set it up explicitly.
        Nothing -> error $ "File does not exist: " <> path
        Just (ReadableFile x) -> pure x
        Just (Symlink target) -> readFile target
        Just (UnreadableFile ex) -> throwIO ex

readFileBinary :: HasFS env => FilePath -> RIO env ByteString
readFileBinary = fmap encodeUtf8 . readFileUtf8

writeFileUtf8 :: HasFS env => FilePath -> Text -> RIO env ()
writeFileUtf8 path = writeFile path . normalFile

writeFileExecutable :: HasFS env => FilePath -> Text -> RIO env ()
writeFileExecutable path = writeFile path . executableFile

writeFileUnreadable :: HasFS env => FilePath -> IOException -> RIO env ()
writeFileUnreadable path = writeFile path . UnreadableFile

createFileLink :: HasFS env => FilePath -> FilePath -> RIO env ()
createFileLink target name = writeFile name $ Symlink target

writeFile :: HasFS env => FilePath -> ReadableFile -> RIO env ()
writeFile path' content = do
    path <- getAbsolutePath path'
    modifyFS' $ \fs -> fs { fsFiles = Map.insert path content $ fsFiles fs }

getCurrentDirectory :: HasFS env => RIO env FilePath
getCurrentDirectory = fsCwd <$> readFS'

setCurrentDirectory :: HasFS env => FilePath -> RIO env ()
setCurrentDirectory cwd = modifyFS' $ \fs -> fs { fsCwd = cwd }

doesPathExist :: HasFS env => FilePath -> RIO env Bool
doesPathExist path' = do
    path <- getAbsolutePath path'
    Map.member path . fsFiles <$> readFS'

doesFileExist :: HasFS env => FilePath -> RIO env Bool
doesFileExist path' =
    (\isPath isDirectory -> isPath && not isDirectory)
        <$> doesPathExist path'
        <*> doesDirectoryExist path'

doesDirectoryExist :: HasFS env => FilePath -> RIO env Bool
doesDirectoryExist path' = do
    path <- getAbsolutePath path'
    let prefix = addTrailingPathSeparator path
    not . null <$> getPrefixed prefix

isFileExecutable :: HasFS env => FilePath -> RIO env Bool
isFileExecutable = fmap (Directory.executable . snd) . readFile

isFileSymbolicLink :: HasFS env => FilePath -> RIO env Bool
isFileSymbolicLink path' = do
    path <- getAbsolutePath path'
    maybe False check . Map.lookup path . fsFiles <$> readFS'
  where
    check = \case
        Symlink _ -> True
        _ -> False

listDirectory :: HasFS env => FilePath -> RIO env [FilePath]
listDirectory path' = do
    path <- getAbsolutePath path'
    let prefix = addTrailingPathSeparator path
    filter (not . null) . map (dropPrefix prefix) <$> getPrefixed prefix

getAbsolutePath :: HasFS env => FilePath -> RIO env FilePath
getAbsolutePath path
    | isAbsolute path = pure path
    | otherwise = do
        FS' {..} <- readFS'
        pure $ fsCwd </> path

getPrefixed :: HasFS env => String -> RIO env [FilePath]
getPrefixed prefix = do
    paths <- Map.keys . fsFiles <$> readFS'
    pure $ filter (prefix `isPrefixOf`) paths
