{-# LANGUAGE UndecidableInstances #-}

module Restyler.Capabilities.System.State
    ( StateSystem(..)
    , FS
    , HasFS(..)
    , buildFS
    , addNormalFile
    , addExecutableFile
    , addSymlink
    , addUnreadableFile
    )
where

import Restyler.Prelude

import Control.Lens ((%=))
import Control.Monad.State
import qualified Data.Map.Strict as Map
import RIO.FilePath (addTrailingPathSeparator, isAbsolute, (</>))
import RIO.List (dropPrefix, isPrefixOf)
import Restyler.App.Error
import Restyler.Capabilities.System
import qualified System.Directory as Directory

newtype StateSystem m a = StateSystem
    { unStateSystem :: m a
    }
    deriving newtype (Applicative, Functor, Monad, MonadError e, MonadState s)

instance (MonadError AppError m, MonadState env m, HasFS env)
    => MonadSystem (StateSystem m) where
    getCurrentDirectory = fsCwd <$> readFS

    setCurrentDirectory cwd = modifyFS $ \fs -> fs { fsCwd = cwd }

    doesFileExist path' =
        (\isPath isDirectory -> isPath && not isDirectory)
            <$> doesPathExist path'
            <*> doesDirectoryExist path'

    doesDirectoryExist path' = do
        path <- getAbsolutePath path'
        let prefix = addTrailingPathSeparator path
        not . null <$> getPrefixed prefix

    isFileExecutable = fmap (Directory.executable . snd) . readFile'

    isFileSymbolicLink path' = do
        path <- getAbsolutePath path'
        maybe False check . Map.lookup path . fsFiles <$> readFS
      where
        check = \case
            Symlink _ -> True
            _ -> False

    listDirectory path' = do
        path <- getAbsolutePath path'
        let prefix = addTrailingPathSeparator path
        filter (not . null) . map (dropPrefix prefix) <$> getPrefixed prefix

    readFile = fmap fst . readFile'

    readFileBS = fmap encodeUtf8 . readFile

    writeFile = addNormalFile


data FS = FS
    { fsCwd :: FilePath
    , fsFiles :: Map FilePath ReadableFile
    }

data ReadableFile
    = ReadableFile (Text, Directory.Permissions)
    | Symlink FilePath
    | UnreadableFile IOException

class HasFS env where
    fsL :: Lens' env FS

instance HasFS FS where
    fsL = id

-- | Build an 'FS' with @/@ as current directory and no files present
buildFS :: FS
buildFS = FS { fsCwd = "/", fsFiles = Map.empty }

readFS :: (MonadState env m, HasFS env) => m FS
readFS = gets $ view fsL

modifyFS :: (MonadState env m, HasFS env) => (FS -> FS) -> m ()
modifyFS f = fsL %= f

addNormalFile :: (MonadState env m, HasFS env) => FilePath -> Text -> m ()
addNormalFile path content = addFile path $ ReadableFile
    ( content
    , Directory.emptyPermissions
        { Directory.readable = True
        , Directory.writable = True
        , Directory.executable = False
        , Directory.searchable = True
        }
    )

addExecutableFile :: (MonadState env m, HasFS env) => FilePath -> Text -> m ()
addExecutableFile path content = addFile path $ ReadableFile
    ( content
    , Directory.emptyPermissions
        { Directory.readable = True
        , Directory.writable = True
        , Directory.executable = True
        , Directory.searchable = True
        }
    )

addSymlink :: (MonadState env m, HasFS env) => FilePath -> FilePath -> m ()
addSymlink src dst = addFile dst $ Symlink src

addUnreadableFile
    :: (MonadState env m, HasFS env) => FilePath -> IOException -> m ()
addUnreadableFile path ex = addFile path $ UnreadableFile ex

addFile :: (MonadState env m, HasFS env) => FilePath -> ReadableFile -> m ()
addFile path' file = do
    path <- getAbsolutePath path'
    modifyFS $ \fs -> fs { fsFiles = Map.insert path file $ fsFiles fs }

readFile'
    :: (MonadError AppError m, MonadState env m, HasFS env)
    => FilePath
    -> m (Text, Directory.Permissions)
readFile' path' = do
    path <- getAbsolutePath path'
    mContent <- Map.lookup path . fsFiles <$> readFS

    case mContent of
        -- We could throw the same error you get from a real read of a missing
        -- file. However, you should be intentional about testing such a
        -- scenario and use writeFileUnreadable to set it up explicitly.
        Nothing -> error $ "File does not exist: " <> path
        Just (ReadableFile x) -> pure x
        Just (Symlink target) -> readFile' target
        Just (UnreadableFile ex) -> throwError $ SystemError ex

doesPathExist :: (MonadState env m, HasFS env) => FilePath -> m Bool
doesPathExist path' = do
    path <- getAbsolutePath path'
    Map.member path . fsFiles <$> readFS

getAbsolutePath :: (MonadState env m, HasFS env) => FilePath -> m FilePath
getAbsolutePath path
    | isAbsolute path = pure path
    | otherwise = do
        FS {..} <- readFS
        pure $ fsCwd </> path

getPrefixed :: (MonadState env m, HasFS env) => String -> m [FilePath]
getPrefixed prefix = do
    paths <- Map.keys . fsFiles <$> readFS
    pure $ filter (prefix `isPrefixOf`) paths
