{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Restyler.Main
    ( restylerMain
    )
where

import Restyler.Prelude

import Restyler.App
import Restyler.App.Run
import Restyler.Capabilities.Docker
import Restyler.Capabilities.Git
import Restyler.Capabilities.GitHub
import Restyler.Capabilities.RemoteFile
import Restyler.Capabilities.System
import Restyler.Model.Comment
import Restyler.Model.Config
import Restyler.Model.PullRequest
import Restyler.Model.PullRequest.Restyled
import Restyler.Model.PullRequest.Status
import Restyler.Model.Restyler (runRestylers)
import Restyler.Options
import System.Exit (die)
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)
import System.IO.Temp (withSystemTempDirectory)

-- | The main entrypoint for the restyler CLI
--
-- Parses command-line options, creates a temporary working directory and runs
-- the restyling process. Application errors are reported to @stderr@ before a
-- non-zero exit.
--
-- See @'parseOptions'@ for usage information.
--
restylerMain :: IO ()
restylerMain = do
    -- Ensure output always works correctly in Docker
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    options <- parseOptions

    withTempDirectory $ \path -> runExceptT $ do
        app <- bootstrapApp options path

        runApp app $ run `catchError` \ex -> do
            traverse_ (sendPullRequestStatus_ . ErrorStatus) $ oJobUrl options
            throwError ex

withTempDirectory :: (FilePath -> IO (Either AppError a)) -> IO a
withTempDirectory f = do
    result <- tryIO $ withSystemTempDirectory "restyler-" f
    innerResult <- either (exitWithAppError . SystemError) pure result
    either exitWithAppError pure innerResult

run
    :: ( MonadSystem m
       , MonadDocker m
       , MonadGit m
       , MonadGitHub m
       , MonadLogger m
       , MonadReader App m
       , MonadRemoteFile m
       )
    => m ()
run = do
    unlessM configEnabled $ exitWithInfo "Restyler disabled by config"

    traverse_ fetchRemoteFile =<< asks (cRemoteFiles . appConfig)

    unlessM restyle $ do
        clearRestyledComments
        closeRestyledPullRequest
        sendPullRequestStatus NoDifferencesStatus
        exitWithInfo "No style differences found"

    whenM isAutoPush $ do
        updateOriginalPullRequest
        exitWithInfo "Pushed to original PR"

    mRestyledPr <- asks appRestyledPullRequest
    restyledUrl <- case mRestyledPr of
        Just restyledPr -> do
            updateRestyledPullRequest
            pure $ simplePullRequestUrl restyledPr
        Nothing -> do
            restyledPr <- createRestyledPullRequest
            leaveRestyledComment restyledPr
            pure $ pullRequestUrl restyledPr

    sendPullRequestStatus $ DifferencesStatus restyledUrl
    logInfoN "Restyling successful"

configEnabled :: MonadReader App m => m Bool
configEnabled = asks $ cEnabled . appConfig

restyle
    :: ( MonadSystem m
       , MonadDocker m
       , MonadGit m
       , MonadLogger m
       , MonadReader App m
       )
    => m Bool
restyle = do
    config <- asks appConfig
    pullRequest <- asks appPullRequest
    pullRequestPaths <- changedPaths $ pullRequestBaseRef pullRequest
    runRestylers (cRestylers config) pullRequestPaths
    not . null <$> changedPaths (pullRequestLocalHeadRef pullRequest)

isAutoPush :: MonadReader App m => m Bool
isAutoPush = do
    isAuto <- asks $ cAuto . appConfig
    pullRequest <- asks appPullRequest
    pure $ isAuto && not (pullRequestIsFork pullRequest)

exitWithAppError :: AppError -> IO a
exitWithAppError = \case
    PullRequestFetchError e -> die $ format
        [ "We had trouble fetching your Pull Request from GitHub:"
        , showGitHubError e
        ]
    PullRequestCloneError e ->
        die $ format
            ["We had trouble cloning your Pull Request branch:" <> show e]
    ConfigurationError msg ->
        die $ format ["We had trouble with your " <> configPath <> ":", msg]
    DockerError e ->
        die $ format ["The restyler container exited non-zero:", show e]
    GitError e ->
        die $ format ["We had trouble running a git command:", show e]
    GitHubError e -> die $ format
        ["We had trouble communicating with GitHub:", showGitHubError e]
    SystemError e ->
        die $ format ["We had trouble running a system command:", show e]
    RemoteFileError e ->
        die $ format ["We had trouble fetching a remote file:", show e]
    OtherError e ->
        die $ format ["We encountered an unexpected exception:", show e]
  where
    format = ("[Error] " <>) . dropWhileEnd isSpace . unlines

    -- This relies on @'HttpException'@ not leaking anything sensitive for
    -- our use-cases, which is true for now (e.g. it masks @Authorization@
    -- headers).
    showGitHubError :: Error -> String
    showGitHubError (HTTPError e) = "HTTP exception: " <> show e
    showGitHubError (ParseError e) = "Unable to parse response: " <> unpack e
    showGitHubError (JsonError e) = "Malformed response: " <> unpack e
    showGitHubError (UserError e) = "User error: " <> unpack e

exitWithInfo :: (MonadSystem m, MonadLogger m) => Text -> m ()
exitWithInfo msg = do
    logInfoN msg
    exitSuccess
