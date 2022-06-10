module Restyler.App
    ( AppT
    , runAppT
    , App(..)
    , StartupApp(..)
    , bootstrapApp
    ) where

import Restyler.Prelude

import Blammo.Logging.Simple (newLoggerEnv)
import Conduit (runResourceT, sinkFile)
import GitHub.Auth
import GitHub.Request
import GitHub.Request.Display
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (Request)
import qualified RIO.Directory as Directory
import Restyler.App.Class
import Restyler.App.Error
import Restyler.Config
import Restyler.Git
import Restyler.Options
import Restyler.PullRequest
import Restyler.RestyledPullRequest
import Restyler.Setup
import Restyler.Statsd (HasStatsClient(..), StatsClient)
import qualified System.Exit as Exit
import qualified System.Process as Process

newtype AppT app m a = AppT
    { unAppT :: ReaderT app (LoggingT m) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadThrow
        , MonadIO
        , MonadUnliftIO
        , MonadLogger
        , MonadReader app
        )

instance MonadUnliftIO m => MonadSystem (AppT app m) where
    getCurrentDirectory = do
        logDebug "getCurrentDirectory"
        appIO SystemError Directory.getCurrentDirectory

    setCurrentDirectory path = do
        logDebug $ "setCurrentDirectory" :# ["path" .= path]
        appIO SystemError $ Directory.setCurrentDirectory path

    doesFileExist path = do
        logDebug $ "doesFileExist" :# ["path" .= path]
        appIO SystemError $ Directory.doesFileExist path

    doesDirectoryExist path = do
        logDebug $ "doesDirectoryExist" :# ["path" .= path]
        appIO SystemError $ Directory.doesDirectoryExist path

    isFileExecutable path = do
        logDebug $ "isFileExecutable" :# ["path" .= path]
        appIO SystemError
            $ Directory.executable
            <$> Directory.getPermissions path

    isFileSymbolicLink path = do
        logDebug $ "isFileSymbolicLink" :# ["path" .= path]
        appIO SystemError $ Directory.pathIsSymbolicLink path

    listDirectory path = do
        logDebug $ "listDirectory" :# ["path" .= path]
        appIO SystemError $ Directory.listDirectory path

    readFile path = do
        logDebug $ "readFile: " :# ["path" .= path]
        appIO SystemError $ readFileUtf8 path

    readFileBS path = do
        logDebug $ "readFileBS" :# ["path" .= path]
        appIO SystemError $ readFileBinary path

    writeFile path content = do
        logDebug $ "writeFile" :# ["path" .= path]
        appIO SystemError $ writeFileUtf8 path content

instance MonadUnliftIO m => MonadProcess (AppT app m) where
    callProcess cmd args = do
        -- N.B. this includes access tokens in log messages when used for
        -- git-clone. That's acceptable because:
        --
        -- - These tokens are ephemeral (5 minutes)
        -- - We generally accept secrets in DEBUG messages
        --
        logDebug $ "callProcess" :# ["command" .= cmd, "arguments" .= args]
        appIO SystemError $ Process.callProcess cmd args

    callProcessExitCode cmd args = do
        logDebug
            $ "callProcessExitCode"
            :# ["command" .= cmd, "arguments" .= args]
        ec <- appIO SystemError $ Process.withCreateProcess proc $ \_ _ _ p ->
            Process.waitForProcess p
        logDebug
            $ "callProcessExitCode"
            :# [ "command" .= cmd
               , "arguments" .= args
               , "exitCode" .= exitCodeInt ec
               ]
        pure ec
        where proc = (Process.proc cmd args) { Process.delegate_ctlc = True }

    readProcess cmd args stdin' = do
        logDebug
            $ "readProcess"
            :# ["command" .= cmd, "arguments" .= args, "stdin" .= stdin']
        output <- appIO SystemError $ Process.readProcess cmd args stdin'
        logDebug
            $ "readProcess"
            :# [ "command" .= cmd
               , "arguments" .= args
               , "stdin" .= stdin'
               , "output" .= output
               ]
        pure output

instance MonadUnliftIO m => MonadExit (AppT app m) where
    exitSuccess = do
        logDebug "exitSuccess"
        appIO SystemError Exit.exitSuccess

instance MonadUnliftIO m => MonadDownloadFile (AppT app m) where
    downloadFile url path = do
        appIO HttpError $ do
            request <- parseRequestThrow $ unpack url
            runResourceT $ httpSink request $ \_ -> sinkFile path

instance (MonadUnliftIO m, HasOptions app) => MonadGitHub (AppT app m) where
    runGitHub req = do
        -- TODO
        -- logDebug $ "GitHub request" <> display (displayGitHubRequest req)
        auth <- OAuth . encodeUtf8 . oAccessToken <$> view optionsL
        result <- liftIO $ do
            mgr <- getGlobalManager
            executeRequestWithMgr mgr auth req
        either (throwIO . GitHubError (displayGitHubRequest req)) pure result

runAppT :: MonadIO m => HasLogger app => app -> AppT app m a -> m a
runAppT app f = runLoggerLoggingT app $ runReaderT (unAppT f) app

appIO :: MonadUnliftIO m => (IOException -> AppError) -> IO a -> m a
appIO f = mapAppError f . liftIO

data StartupApp = StartupApp
    { appLogger :: Logger
    , appOptions :: Options
    , appWorkingDirectory :: FilePath
    , appStatsClient :: StatsClient
    }

instance HasLogger StartupApp where
    loggerL = lens appLogger $ \x y -> x { appLogger = y }

instance HasOptions StartupApp where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasWorkingDirectory StartupApp where
    workingDirectoryL =
        lens appWorkingDirectory $ \x y -> x { appWorkingDirectory = y }

instance HasStatsClient StartupApp where
    statsClientL = lens appStatsClient $ \x y -> x { appStatsClient = y }

data App = App
    { appApp :: StartupApp
    , appConfig :: Config
    , appPullRequest :: PullRequest
    , appRestyledPullRequest :: Maybe RestyledPullRequest
    }

appL :: Lens' App StartupApp
appL = lens appApp $ \x y -> x { appApp = y }

instance HasLogger App where
    loggerL = appL . loggerL

instance HasOptions App where
    optionsL = appL . optionsL

instance HasWorkingDirectory App where
    workingDirectoryL = appL . workingDirectoryL

instance HasConfig App where
    configL = lens appConfig $ \x y -> x { appConfig = y }

instance HasPullRequest App where
    pullRequestL = lens appPullRequest $ \x y -> x { appPullRequest = y }

instance HasRestyledPullRequest App where
    restyledPullRequestL =
        lens appRestyledPullRequest $ \x y -> x { appRestyledPullRequest = y }

instance MonadUnliftIO m => MonadGit (AppT App m) where
    gitPush branch = callProcess "git" ["push", "origin", branch]
    gitPushForce branch =
        callProcess "git" ["push", "--force", "origin", branch]
    gitDiffNameOnly mRef = do
        let args = ["diff", "--name-only"] <> maybeToList mRef
        lines <$> readProcess "git" args ""
    gitCommitAll msg = do
        callProcess "git" ["commit", "-a", "--message", msg]
        dropWhileEnd isSpace <$> readProcess "git" ["rev-parse", "HEAD"] ""
    gitCheckout branch = do
        callProcess "git" ["checkout", "--no-progress", "-b", branch]

bootstrapApp
    :: (MonadThrow m, MonadUnliftIO m)
    => Options
    -> FilePath
    -> StatsClient
    -> m App
bootstrapApp options path statsClient = do
    logger <- newLoggerEnv

    let app = StartupApp
            { appLogger = logger
            , appOptions = options
            , appWorkingDirectory = path
            , appStatsClient = statsClient
            }
        toApp (pullRequest, mRestyledPullRequest, config) = App
            { appApp = app
            , appPullRequest = pullRequest
            , appRestyledPullRequest = mRestyledPullRequest
            , appConfig = config
            }

    runAppT app $ toApp <$> restylerSetup
