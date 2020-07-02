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
-- - Directories work a bit like S3: they're just a common prefix shared by
--   multiple keys. This means an empty directory cannot be modeled. You can
--   make @'doesDirectoryExist'@ work with a file ending in @/@, but using
--   @'listDirectory'@ on it will return @[""]@, which is not right.
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
    , getCurrentDirectory
    , setCurrentDirectory
    , doesFileExist
    , doesDirectoryExist
    , isFileExecutable
    , listDirectory
    )
where

import RIO hiding (readFileBinary, readFileUtf8, writeFileUtf8)

import qualified Data.Map.Strict as Map
import RIO.FilePath ((</>))
import RIO.List (dropPrefix, dropWhileEnd, isPrefixOf)
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
        Just (UnreadableFile ex) -> throwIO ex

readFileBinary :: HasFS env => FilePath -> RIO env ByteString
readFileBinary = fmap encodeUtf8 . readFileUtf8

writeFileUtf8 :: HasFS env => FilePath -> Text -> RIO env ()
writeFileUtf8 path = writeFile path . normalFile

writeFileExecutable :: HasFS env => FilePath -> Text -> RIO env ()
writeFileExecutable path = writeFile path . executableFile

writeFileUnreadable :: HasFS env => FilePath -> IOException -> RIO env ()
writeFileUnreadable path = writeFile path . UnreadableFile

writeFile :: HasFS env => FilePath -> ReadableFile -> RIO env ()
writeFile path' content = do
    path <- getAbsolutePath path'
    modifyFS' $ \fs -> fs { fsFiles = Map.insert path content $ fsFiles fs }

getCurrentDirectory :: HasFS env => RIO env FilePath
getCurrentDirectory = fsCwd <$> readFS'

setCurrentDirectory :: HasFS env => FilePath -> RIO env ()
setCurrentDirectory cwd = modifyFS' $ \fs -> fs { fsCwd = cwd }

doesFileExist :: HasFS env => FilePath -> RIO env Bool
doesFileExist path' = do
    path <- getAbsolutePath path'
    Map.member path . fsFiles <$> readFS'

doesDirectoryExist :: HasFS env => FilePath -> RIO env Bool
doesDirectoryExist path' = not . null <$> listDirectory path'

isFileExecutable :: HasFS env => FilePath -> RIO env Bool
isFileExecutable = fmap (Directory.executable . snd) . readFile

listDirectory :: HasFS env => FilePath -> RIO env [FilePath]
listDirectory path' = do
    path <- getAbsolutePath path'
    let prefix = dropWhileEnd (== '/') path <> "/"
    paths <- Map.keys . fsFiles <$> readFS'
    pure $ map (dropPrefix prefix) $ filter (prefix `isPrefixOf`) paths

getAbsolutePath :: HasFS env => FilePath -> RIO env FilePath
getAbsolutePath path
    | "/" `isPrefixOf` path = pure path
    | otherwise = do
        FS' {..} <- readFS'
        pure $ fsCwd </> path
