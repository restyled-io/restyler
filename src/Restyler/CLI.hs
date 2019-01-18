{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.CLI
    ( restylerCLI
    ) where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import Restyler.App
import Restyler.Logger
import Restyler.Main
import Restyler.Config
import Restyler.PullRequest.Status
import Restyler.Options
import Restyler.Setup
import System.Exit (die)
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)
import System.IO.Temp (withSystemTempDirectory)

-- | The main entrypoint for the restyler CLI
--
-- Parses command-line options, creates a temporary working directory and runs
-- the restyling process. Application errors are reported to @stderr@ before a
-- non-zero exit. The PullRequest is sent an error status as long as it has been
-- initialized by the point of the failure.
--
-- See @'parseOptions'@ for usage information.
--
restylerCLI :: IO ()
restylerCLI = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    options <- parseOptions
    withTempDirectory $ \path -> runExceptT $ do
        app <- bootstrapApp options path restylerSetup
        runApp app $ restylerMain `catchError` \ex -> do
            traverse_ (sendPullRequestStatus_ . ErrorStatus) $ oJobUrl options
            throwError ex

withTempDirectory :: (FilePath -> IO (Either AppError a)) -> IO a
withTempDirectory f = do
    result <- tryIO $ withSystemTempDirectory "restyler-" f
    innerResult <- either (dieAppError . SystemError) pure result
    either dieAppError pure innerResult

bootstrapApp
    :: MonadIO m
    => Options
    -> FilePath
    -> AppT m (PullRequest, Maybe SimplePullRequest, Config)
    -> ExceptT AppError m App
bootstrapApp options@Options {..} path f = do
    let
        tempApp = App
            { appLogLevel = oLogLevel
            , appLogColor = oLogColor
            , appAccessToken = oAccessToken
            , appPullRequest = error "Bootstrap appPullRequest forced"
            , appRestyledPullRequest = Nothing
            , appConfig = error "Bootstrap appConfig forced"
            , appOptions = options
            , appWorkingDirectory = path
            }

    runApp tempApp $ do
        (pullRequest, mRestyledPullRequest, config) <- f

        pure tempApp
            { appPullRequest = pullRequest
            , appRestyledPullRequest = mRestyledPullRequest
            , appConfig = config
            }

runApp :: MonadIO m => App -> AppT m a -> ExceptT AppError m a
runApp app = runAppLoggingT app . flip runReaderT app . runAppT

-- brittany-next-binding --columns 90

dieAppError :: AppError -> IO a
dieAppError = die . format . \case
    PullRequestFetchError e ->
        ["We had trouble fetching your Pull Request from GitHub:", showGitHubError e]
    PullRequestCloneError e ->
        ["We had trouble cloning your Pull Request branch:" <> show e]
    ConfigurationError ex ->
        [ "We had trouble with your "
            <> configPath
            <> ": "
            <> Yaml.prettyPrintParseException ex
        , "Please see https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml"
        ]
    DockerError e -> ["The restyler container exited non-zero:", show e]
    GitHubError e -> ["We had trouble communicating with GitHub:", showGitHubError e]
    SystemError e -> ["We had trouble running a system command:", show e]
    HttpError e -> ["We had trouble performing an HTTP request:", show e]
    OtherError e -> ["We encountered an unexpected exception:", show e]
  where
    format msg = "[Error] " <> dropWhileEnd isSpace (unlines msg)
    showGitHubError (HTTPError e) = "HTTP exception: " <> show e
    showGitHubError (ParseError e) = "Unable to parse response: " <> unpack e
    showGitHubError (JsonError e) = "Malformed response: " <> unpack e
    showGitHubError (UserError e) = "User error: " <> unpack e
