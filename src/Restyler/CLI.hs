{-# LANGUAGE LambdaCase #-}

module Restyler.CLI
    ( restylerCLI
    ) where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import Restyler.App
import Restyler.Config
import Restyler.Logger
import Restyler.Main
import Restyler.Options
import Restyler.PullRequest.Status
import Restyler.Setup
import System.Exit (die)

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
    options@Options {..} <- parseOptions

    withRestylerDirectory $ \path -> do
        app <- withTempApp options path
            $ \tempApp -> uncurry3 (tempAppToApp tempApp) <$> restylerSetup

        runRIO app $ restylerMain `catchAny` \ex -> do
            traverse_ (sendPullRequestStatus_ . ErrorStatus) oJobUrl
            throwIO ex

-- | Run in a prefixed temporary directory
--
-- Also ensures all exceptions will flow through @'dieAppError'@.
--
withRestylerDirectory :: (FilePath -> IO a) -> IO a
withRestylerDirectory f =
    withSystemTempDirectory "restyler-" f
        `catches` [Handler dieAppError, Handler $ dieAppError . SystemError]

-- | Used for running @'RIO'@ actions to construct our @'App'@
--
-- Holds onto a partial @'App'@ value so we can re-use many @Has@ instances from
-- it. But specifically does not have instances for @'HasConfig'@,
-- @'HasPullRequest'@, or @'HasRestyledPullRequest'@, making it safe that we
-- hold onto a partial version of @'App'@.
--
newtype TempApp = TempApp { unTempApp :: App }

appL :: Lens' TempApp App
appL = lens unTempApp $ \_ y -> TempApp y

instance HasOptions TempApp where
    optionsL = appL . optionsL

instance HasWorkingDirectory TempApp where
    workingDirectoryL = appL . workingDirectoryL

runTempApp :: RIO App a -> RIO TempApp a
runTempApp f = do
    TempApp app <- ask
    runRIO app f

instance HasSystem TempApp where
    getCurrentDirectory = runTempApp getCurrentDirectory
    setCurrentDirectory = runTempApp . setCurrentDirectory
    doesFileExist = runTempApp . doesFileExist
    readFile = runTempApp . readFile

instance HasProcess TempApp where
    callProcess cmd = runTempApp . callProcess cmd
    readProcess cmd args = runTempApp . readProcess cmd args

instance HasGitHub TempApp where
    runGitHub = runTempApp . runGitHub

tempAppToApp
    :: TempApp -> PullRequest -> Maybe SimplePullRequest -> Config -> App
tempAppToApp (TempApp app) pullRequest mRestyledPullRequest config = app
    { appPullRequest = pullRequest
    , appRestyledPullRequest = mRestyledPullRequest
    , appConfig = config
    }

withTempApp
    :: MonadUnliftIO m
    => Options
    -> FilePath
    -> (TempApp -> RIO TempApp a)
    -> m a
withTempApp options path f = withRestylerLogFunc options $ \lf -> do
    let
        tempApp = TempApp App
            { appLogFunc = lf
            , appAccessToken = oAccessToken options
            , appPullRequest = error ""
            , appRestyledPullRequest = error ""
            , appConfig = error ""
            , appOptions = options
            , appWorkingDirectory = path
            }

    runRIO tempApp $ f tempApp

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
