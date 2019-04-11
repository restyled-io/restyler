module Restyler.App
    ( App(..)
    , bootstrapApp

    -- * Application errors
    , AppError(..)
    , mapAppError
    ) where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import qualified Data.Yaml as Yaml
import GitHub.Request
import GitHub.Request.Display
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (Request)
import Restyler.App.Class
import Restyler.Logger
import Restyler.Config
import Restyler.Options
import qualified RIO.Directory as Directory
import qualified System.Exit as Exit
import qualified System.Process as Process

-- | All possible application error conditions
data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigurationError Yaml.ParseException
    -- ^ We couldn't load a @.restyled.yaml@
    | DockerError IOException
    -- ^ Error running a @docker@ operation
    | GitHubError Error
    -- ^ We encountered a GitHub API error during restyling
    | SystemError IOException
    -- ^ Trouble reading a file or etc
    | HttpError IOException
    -- ^ Trouble performing some HTTP request
    | OtherError IOException
    -- ^ A minor escape hatch for @'IOException'@s
    deriving Show

instance Exception AppError

-- | Run a computation, and modify any thrown exceptions to @'AppError'@s
mapAppError :: (MonadUnliftIO m, Exception e) => (e -> AppError) -> m a -> m a
mapAppError f = (`catch` throwIO . f)

-- | Run an @'IO'@ computation and capture @'IOException'@s to the given type
appIO :: MonadUnliftIO m => (IOException -> AppError) -> IO a -> m a
appIO f = mapAppError f . liftIO

-- | App environment that is ready immediately
--
-- This is used to bootstrap the main @'App'@ type, which will re-use its
-- instances and provide those that only work in the complete environment.
--
data TempApp = TempApp
    { appLogFunc :: LogFunc
    , appOptions :: Options
    , appWorkingDirectory :: FilePath
    }

instance HasLogFunc TempApp where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasOptions TempApp where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasWorkingDirectory TempApp where
    workingDirectoryL = lens appWorkingDirectory $ \x y ->
        x { appWorkingDirectory = y }

instance HasSystem TempApp where
    getCurrentDirectory = do
        logDebug "getCurrentDirectory"
        appIO SystemError Directory.getCurrentDirectory

    setCurrentDirectory path = do
        logDebug $ "setCurrentDirectory: " <> displayShow path
        appIO SystemError $ Directory.setCurrentDirectory path

    doesFileExist path = do
        logDebug $ "doesFileExist: " <> displayShow path
        appIO SystemError $ Directory.doesFileExist path

    readFile path = do
        logDebug $ "readFile: " <> displayShow path
        appIO SystemError $ readFileUtf8 path

instance HasProcess TempApp where
    callProcess cmd args = do
        -- N.B. this includes access tokens in log messages when used for
        -- git-clone. That's acceptable because:
        --
        -- - These tokens are ephemeral (5 minutes)
        -- - We generally accept secrets in DEBUG messages
        --
        logDebug $ "call: " <> fromString cmd <> " " <> displayShow args
        appIO SystemError $ Process.callProcess cmd args

    readProcess cmd args stdin' = do
        logDebug $ "read: " <> fromString cmd <> " " <> displayShow args
        output <- appIO SystemError $ Process.readProcess cmd args stdin'
        output <$ logDebug ("output: " <> fromString output)

instance HasGitHub TempApp where
    runGitHub req = do
        logDebug $ "GitHub request: " <> displayGitHubRequest req
        auth <- OAuth . encodeUtf8 . oAccessToken <$> view optionsL
        result <- appIO OtherError $ do
            mgr <- getGlobalManager
            executeRequestWithMgr mgr auth req
        either (throwIO . GitHubError) pure result

bootstrapApp
    :: MonadIO m
    => Options
    -> FilePath
    -> RIO TempApp (PullRequest, Maybe SimplePullRequest, Config)
    -> m App
bootstrapApp options@Options {..} path = runRIO tempApp . fmap toApp
  where
    tempApp = TempApp
        { appLogFunc = restylerLogFunc options
        , appOptions = options
        , appWorkingDirectory = path
        }

    -- NB. Could be uncurry3 (App tempApp)
    toApp (pullRequest, mRestyledPullRequest, config) = App
        { appApp = tempApp
        , appPullRequest = pullRequest
        , appRestyledPullRequest = mRestyledPullRequest
        , appConfig = config
        }

-- | Application environment
data App = App
    { appApp :: TempApp
    , appConfig :: Config
    , appPullRequest :: PullRequest
    , appRestyledPullRequest :: Maybe SimplePullRequest
    }

instance HasLogFunc App where
    logFuncL = appL . logFuncL

instance HasOptions App where
    optionsL = appL . optionsL

instance HasWorkingDirectory App where
    workingDirectoryL = appL . workingDirectoryL

instance HasConfig App where
    configL = lens appConfig $ \x y -> x { appConfig = y }

instance HasPullRequest App where
    pullRequestL = lens appPullRequest $ \x y -> x { appPullRequest = y }

instance HasRestyledPullRequest App where
    restyledPullRequestL = lens appRestyledPullRequest $ \x y ->
        x { appRestyledPullRequest = y }

instance HasSystem App where
    getCurrentDirectory = runApp getCurrentDirectory
    setCurrentDirectory = runApp . setCurrentDirectory
    doesFileExist = runApp . doesFileExist
    readFile = runApp . readFile

instance HasExit App where
    exitSuccess = do
        logDebug "exitSuccess"
        appIO SystemError Exit.exitSuccess

instance HasProcess App where
    callProcess cmd = runApp . callProcess cmd
    readProcess cmd args = runApp . readProcess cmd args

instance HasDownloadFile App where
    downloadFile url path = do
        logDebug $ "HTTP GET: " <> displayShow url <> " => " <> displayShow path
        appIO HttpError $ do
            request <- parseRequest $ unpack url
            runResourceT $ httpSink request $ \_ -> sinkFile path

instance HasGitHub App where
    runGitHub = runApp . runGitHub

appL :: Lens' App TempApp
appL = lens appApp $ \x y -> x { appApp = y }

runApp :: RIO TempApp a -> RIO App a
runApp f = do
    app <- asks appApp
    runRIO app f
