module Restyler.App.Class
    (
    -- * Data
      HasOptions(..)
    , HasConfig(..)
    , HasPullRequest(..)
    , HasRestyledPullRequest(..)
    , HasWorkingDirectory(..)

    -- * Capabilities
    , HasSystem(..)
    , HasExit(..)
    , exitWithInfo
    , HasProcess(..)
    , HasDownloadFile(..)
    , HasGitHub(..)
    , runGitHubFirst
    , runGitHub_
    ) where

import Restyler.Prelude

import Data.Aeson
import GitHub.Data.Request
import Restyler.Config
import Restyler.Options
import Restyler.PullRequest
import qualified RIO.Vector as V

class HasWorkingDirectory env where
    workingDirectoryL :: Lens' env FilePath

class HasSystem env where
    getCurrentDirectory :: RIO env FilePath

    setCurrentDirectory :: FilePath -> RIO env ()

    doesFileExist :: FilePath -> RIO env Bool

    readFile :: FilePath -> RIO env Text

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
    runGitHub :: FromJSON a => Request k a -> RIO env a

-- | Fetch the first page using @'runGitHub'@, return the first item
runGitHubFirst
    :: (HasGitHub env, FromJSON a)
    => (FetchCount -> Request k (Vector a))
    -> RIO env (Maybe a)
runGitHubFirst f = (V.!? 0) <$> runGitHub (f 1)

-- | @'void' . 'runGitHub'@
runGitHub_ :: (HasGitHub env, FromJSON a) => Request k a -> RIO env ()
runGitHub_ = void . runGitHub
