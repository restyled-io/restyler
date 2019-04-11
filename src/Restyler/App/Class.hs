{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

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
    , HasProcess(..)
    , HasDownloadFile(..)
    , HasGitHub(..)
    , runGitHubFirst
    , runGitHub_

    -- * Re-exports
    -- |
    --
    -- This is useful because it defines what GitHub APIs we use and takes care
    -- of the hidings we'd otherwise need to do at every import site.
    --
    , module GitHub.Endpoints.Issues.Comments
    , module GitHub.Endpoints.Issues.Labels
    , module GitHub.Endpoints.PullRequests
    , module GitHub.Endpoints.PullRequests.ReviewRequests
    , module GitHub.Endpoints.Repos.Statuses
    ) where

import Restyler.Prelude

import GitHub.Endpoints.Issues.Comments hiding (comment, comments)
import GitHub.Endpoints.Issues.Labels
import GitHub.Endpoints.PullRequests hiding (pullRequest)
import GitHub.Endpoints.PullRequests.ReviewRequests
import GitHub.Endpoints.Repos.Statuses
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
    exitSuccess :: RIO env ()

class HasProcess env where
    callProcess :: String -> [String] -> RIO env ()
    readProcess :: String -> [String] -> String -> RIO env String

-- class HasProcess env => HasGit env where

class HasDownloadFile env where
    downloadFile :: Text -> FilePath -> RIO env ()

class HasGitHub env where
    runGitHub :: Request k a -> RIO env a

-- | Fetch the first page using @'runGitHub'@, return the first item
runGitHubFirst
    :: HasGitHub env
    => (FetchCount -> Request k (Vector a))
    -> RIO env (Maybe a)
runGitHubFirst f = (V.!? 0) <$> runGitHub (f 1)

-- | @'void' . 'runGitHub'@
runGitHub_ :: HasGitHub env => Request k a -> RIO env ()
runGitHub_ = void . runGitHub
