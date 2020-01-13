module Restyler.App.Class
    ( HasWorkingDirectory(..)
    , HasSystem(..)
    , HasExit(..)
    , exitWithInfo
    , HasProcess(..)
    , HasDownloadFile(..)

    -- * GitHub
    , HasGitHub(..)
    , runGitHubFirst
    , runGitHub_

    -- ** Higher-level actions
    , getPullRequestLabelNames
    )
where

import Restyler.Prelude

import GitHub.Data (IssueLabel(..), PullRequest(..))
import GitHub.Data.Request
import GitHub.Endpoints.Issues.Labels (labelsOnIssueR)
import GitHub.Request
import Restyler.PullRequest
import qualified RIO.Vector as V

class HasWorkingDirectory env where
    workingDirectoryL :: Lens' env FilePath

class HasSystem env where
    getCurrentDirectory :: RIO env FilePath

    setCurrentDirectory :: FilePath -> RIO env ()

    doesFileExist :: FilePath -> RIO env Bool

    readFile :: FilePath -> RIO env Text

    readFileBS :: FilePath -> RIO env ByteString

    writeFile :: FilePath -> Text -> RIO env ()

class HasExit env where
    exitSuccess :: RIO env a

exitWithInfo :: (HasLogFunc env, HasExit env) => Utf8Builder -> RIO env a
exitWithInfo msg = do
    logInfo msg
    exitSuccess

class HasProcess env where
    callProcess :: String -> [String] -> RIO env ()

    callProcessExitCode :: String -> [String] -> RIO env ExitCode

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

getPullRequestLabelNames
    :: HasGitHub env => PullRequest -> RIO env (Vector (Name IssueLabel))
getPullRequestLabelNames pullRequest = do
    labels <- handleAny (const $ pure mempty) $ runGitHub $ labelsOnIssueR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestIssueId pullRequest)
        FetchAll
    pure $ labelName <$> labels
