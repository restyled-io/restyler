{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Restyler.Main
    ( restylerMain
    )
where

import Restyler.Prelude

import Restyler.App.Run
import Restyler.Comments
import Restyler.Core
import Restyler.Git
import Restyler.GitHub
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequest.Restyled
import Restyler.PullRequest.Status
import System.Exit (die, exitSuccess)
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)
import System.IO.Temp (withSystemTempDirectory)

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
    innerResult <- either (exitWithAppError . OtherError) pure result
    either exitWithAppError pure innerResult

run :: AppM ()
run = do
    unlessM configEnabled $ exitWithInfo "Restyler disabled by config"

    unlessM runRestyler $ do
        clearRestyledComments
        sendPullRequestStatus NoDifferencesStatus
        exitWithInfo "No style differences found"

    whenM isAutoPush $ do
        updateOriginalPullRequest
        exitWithInfo "Pushed to original PR"

    whenM restyledPullRequestExists $ do
        updateRestyledPullRequest
        exitWithInfo "Pushed to existing restyled branch"

    leaveRestyledComment =<< createRestyledPullRequest
    logInfoN "Restyling successful"

configEnabled :: AppM Bool
configEnabled = asks $ cEnabled . appConfig

runRestyler :: AppM Bool
runRestyler = do
    config <- asks appConfig
    pullRequest <- asks appPullRequest
    pullRequestPaths <- changedPaths $ pullRequestBaseRef pullRequest
    restyle (cRestylers config) pullRequestPaths
    not . null <$> changedPaths (pullRequestLocalHeadRef pullRequest)

isAutoPush :: AppM Bool
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
    GitHubError e -> die $ format
        ["We had trouble communicating with GitHub:", showGitHubError e]
    OtherError e ->
        die $ format ["We encounted an unexpected exception:", show e]
    where format = ("[Error] " <>) . dropWhileEnd isSpace . unlines

exitWithInfo :: Text -> AppM ()
exitWithInfo msg = do
    logInfoN msg
    liftIOApp exitSuccess
