{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Main
    ( restylerMain
    )
where

import Restyler.Prelude

import Restyler.App
import Restyler.App.Run
import Restyler.Model.Comment
import Restyler.Model.Config
import Restyler.Model.PullRequest
import Restyler.Model.PullRequest.Restyled
import Restyler.Model.PullRequest.Status
import Restyler.Model.RemoteFile
import Restyler.Model.Restyler
import Restyler.Model.Restyler.Run
import Restyler.Options
import System.Exit (die)
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)
import System.IO.Temp (withSystemTempDirectory)

data RestyleResult = RestyleResult
    { rrRestylersRan :: [Restyler]
    , rrChangedPaths :: [FilePath]
    }

wasRestyled :: RestyleResult -> Bool
wasRestyled = not . null . rrChangedPaths

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

run :: (HasCallStack, MonadIO m) => AppT m ()
run = do
    unlessM configEnabled $ exitWithInfo "Restyler disabled by config"

    remoteFiles <- asks (cRemoteFiles . appConfig)
    logInfoN $ "Fetching " <> tshow (length remoteFiles) <> " remote file(s)"
    for_ remoteFiles $ \RemoteFile {..} -> downloadFile (getUrl rfUrl) rfPath

    result <- restyle
    unless (wasRestyled result) $ do
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
            pure $ simplePullRequestHtmlUrl restyledPr
        Nothing -> do
            restyledPr <- createRestyledPullRequest $ rrRestylersRan result
            whenM commentsEnabled $ leaveRestyledComment restyledPr
            pure $ pullRequestHtmlUrl restyledPr

    sendPullRequestStatus $ DifferencesStatus restyledUrl
    logInfoN "Restyling successful"

configEnabled :: Monad m => AppT m Bool
configEnabled = asks $ cEnabled . appConfig

commentsEnabled :: Monad m => AppT m Bool
commentsEnabled = asks $ cCommentsEnabled . appConfig

restyle :: MonadIO m => AppT m RestyleResult
restyle = do
    config <- asks appConfig
    pullRequest <- asks appPullRequest
    pullRequestPaths <- changedPaths $ pullRequestBaseRef pullRequest

    RestyleResult
        <$> runRestylers (cRestylers config) pullRequestPaths
        <*> changedPaths (pullRequestLocalHeadRef pullRequest)

changedPaths :: MonadIO m => Text -> AppT m [FilePath]
changedPaths branch = do
    output <- lines
        <$> readProcess "git" ["merge-base", unpack branch, "HEAD"] ""
    let ref = maybe branch pack $ listToMaybe output
    lines <$> readProcess "git" ["diff", "--name-only", unpack ref] ""

isAutoPush :: Monad m => AppT m Bool
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
    ConfigurationError msg -> die $ format
        [ "We had trouble with your " <> configPath <> ":"
        , msg
        , "Please see https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml"
        ]
    DockerError e ->
        die $ format ["The restyler container exited non-zero:", show e]
    GitHubError e -> die $ format
        ["We had trouble communicating with GitHub:", showGitHubError e]
    SystemError e ->
        die $ format ["We had trouble running a system command:", show e]
    HttpError e ->
        die $ format ["We had trouble performing an HTTP request:", show e]
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

exitWithInfo :: MonadIO m => Text -> AppT m ()
exitWithInfo msg = do
    logInfoN msg
    exitSuccess
