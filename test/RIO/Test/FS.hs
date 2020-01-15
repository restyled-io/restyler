-- | Naive implementation of an in-memory filesystem
--
-- This aims to be generic to the @"RIO"@ FS-related functions, but is of course
-- only written to implement our own @'HasSystem'@ class so far.
--
module RIO.Test.FS
    ( HasFS(..)
    , FS
    , build
    , readFileUtf8
    , readFileBinary
    , writeFileUtf8
    , writeFileUnreadable
    , getCurrentDirectory
    , setCurrentDirectory
    , doesFileExist
    )
where

import RIO hiding (readFileBinary, readFileUtf8, writeFileUtf8)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import RIO.FilePath ((</>))
import RIO.List (isPrefixOf)

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
    = ReadableFile Text
    | UnreadableFile IOException

build :: MonadIO m => FilePath -> [(FilePath, Text)] -> m FS
build cwd files = FS <$> newIORef FS'
    { fsCwd = cwd
    , fsFiles = Map.fromList $ map (second ReadableFile) files
    }

readFileUtf8 :: HasFS env => FilePath -> RIO env Text
readFileUtf8 path' = do
    path <- getAbsolutePath path'
    mContent <- Map.lookup path . fsFiles <$> readFS'

    case mContent of
        -- We could throw the same error you get from a real read of a missing
        -- file. However, you should be intentional about testing such a
        -- scenario and use writeFileUnreadable to set it up explicitly.
        Nothing -> error $ "File does not exist: " <> path
        Just (ReadableFile contents) -> pure contents
        Just (UnreadableFile ex) -> throwIO ex

readFileBinary :: HasFS env => FilePath -> RIO env ByteString
readFileBinary = fmap encodeUtf8 . readFileUtf8

writeFileUtf8 :: HasFS env => FilePath -> Text -> RIO env ()
writeFileUtf8 path = writeFile path . ReadableFile

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

-- |
--
-- FIXME. We don't get too-too crazy here in terms of resolving "..", etc; we
-- just consider "/" as denoting an absolute path.
--
getAbsolutePath :: HasFS env => FilePath -> RIO env FilePath
getAbsolutePath path
    | "/" `isPrefixOf` path = pure path
    | otherwise = do
        FS' {..} <- readFS'
        pure $ fsCwd </> path
