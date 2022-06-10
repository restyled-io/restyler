module Restyler.App.Class
    ( HasWorkingDirectory(..)
    , MonadSystem(..)
    , MonadExit(..)
    , exitWithInfo
    , MonadProcess(..)
    , MonadDownloadFile(..)

    -- * GitHub
    , MonadGitHub(..)
    , runGitHubFirst
    , runGitHub_

    -- ** Higher-level actions
    , getPullRequestLabelNames
    ) where

import Restyler.Prelude

import GitHub.Data (IssueLabel(..))
import GitHub.Data.Request
import GitHub.Endpoints.Issues.Labels (labelsOnIssueR)
import GitHub.Request
import qualified RIO.Vector as V
import Restyler.PullRequest

class HasWorkingDirectory env where
    workingDirectoryL :: Lens' env FilePath

class Monad m => MonadSystem m where
    getCurrentDirectory :: m FilePath
    setCurrentDirectory :: FilePath -> m ()
    doesFileExist :: FilePath -> m Bool
    doesDirectoryExist :: FilePath -> m Bool
    isFileExecutable :: FilePath -> m Bool
    isFileSymbolicLink :: FilePath -> m Bool
    listDirectory :: FilePath -> m [FilePath]
    readFile :: FilePath -> m Text
    readFileBS :: FilePath -> m ByteString
    writeFile :: FilePath -> Text -> m ()

class Monad m => MonadExit m where
    exitSuccess :: m a

exitWithInfo :: (MonadLogger m, MonadExit m) => Message -> m a
exitWithInfo msg = do
    logInfo msg
    exitSuccess

class Monad m => MonadProcess m where
    callProcess :: String -> [String] -> m ()
    callProcessExitCode :: String -> [String] -> m ExitCode
    readProcess :: String -> [String] -> String -> m String

class Monad m => MonadDownloadFile m where
    downloadFile :: Text -> FilePath -> m ()

class Monad n => MonadGitHub n where
    runGitHub :: ParseResponse m a => GenRequest m k a -> n a

-- | Fetch the first page using @'runGitHub'@, return the first item
runGitHubFirst
    :: (MonadGitHub n, ParseResponse m (Vector a))
    => (FetchCount -> GenRequest m k (Vector a))
    -> n (Maybe a)
runGitHubFirst f = (V.!? 0) <$> runGitHub (f 1)

-- | @'void' . 'runGitHub'@
runGitHub_ :: (MonadGitHub n, ParseResponse m a) => GenRequest m k a -> n ()
runGitHub_ = void . runGitHub

getPullRequestLabelNames
    :: (MonadUnliftIO m, MonadGitHub m)
    => PullRequest
    -> m (Vector (Name IssueLabel))
getPullRequestLabelNames pullRequest = do
    labels <- handleAny (const $ pure mempty) $ runGitHub $ labelsOnIssueR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestIssueId pullRequest)
        FetchAll
    pure $ labelName <$> labels
