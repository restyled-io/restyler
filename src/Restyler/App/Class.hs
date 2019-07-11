module Restyler.App.Class
    ( HasWorkingDirectory(..)
    , HasSystem(..)
    , HasExit(..)
    , exitWithInfo
    , HasProcess(..)
    , HasDownloadFile(..)
    , HasGitHub(..)
    , runGitHubFirst
    , runGitHub_
    )
where

import Restyler.Prelude

import GitHub.Data.Request
import GitHub.Request
import qualified RIO.Vector as V

class HasWorkingDirectory env where
    workingDirectoryL :: Lens' env FilePath

class HasSystem env where
    getCurrentDirectory :: RIO env FilePath

    setCurrentDirectory :: FilePath -> RIO env ()

    doesFileExist :: FilePath -> RIO env Bool

    readFile :: FilePath -> RIO env Text

    readFileBS :: FilePath -> RIO env ByteString

class HasExit env where
    exitSuccess :: RIO env a

exitWithInfo :: (HasLogFunc env, HasExit env) => Utf8Builder -> RIO env a
exitWithInfo msg = do
    logInfo msg
    exitSuccess

class HasProcess env where
    callProcess :: String -> [String] -> RIO env ()

    readProcess :: String -> [String] -> String -> RIO env String

class HasDownloadFile env where
    downloadFile :: Text -> FilePath -> RIO env ()

class HasGitHub env where
    runGitHub :: ParseResponse m a => GenRequest m k a -> RIO env a

-- | Fetch the first page using @'runGitHub'@, return the first item
runGitHubFirst
    :: (HasGitHub env, ParseResponse m (Vector a))
    => (FetchCount -> GenRequest m k (Vector a))
    -> RIO env (Maybe a)
runGitHubFirst f = (V.!? 0) <$> runGitHub (f 1)

-- | @'void' . 'runGitHub'@
runGitHub_
    :: (HasGitHub env, ParseResponse m a) => GenRequest m k a -> RIO env ()
runGitHub_ = void . runGitHub
