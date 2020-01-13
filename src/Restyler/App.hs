module Restyler.App
    ( App(..)
    , StartupApp(..)
    , bootstrapApp
    ) where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import GitHub.Auth
import GitHub.Request
import GitHub.Request.Display
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (Request)
import Restyler.App.Class
import Restyler.App.Error
import Restyler.Config
import Restyler.Git
import Restyler.Logger
import Restyler.Options
import Restyler.PullRequest
import Restyler.Setup
import qualified RIO.Directory as Directory
import qualified System.Exit as Exit
import qualified System.Process as Process

-- | Environment used for @'RIO'@ actions to load the real @'App'@
data StartupApp = StartupApp
    { appLogFunc :: LogFunc
    -- ^ Log function built based on @--debug@ and @--color@
    , appOptions :: Options
    -- ^ Options passed on the command-line
    , appWorkingDirectory :: FilePath
    -- ^ Temporary working directory we've created
    }

instance HasLogFunc StartupApp where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasOptions StartupApp where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasWorkingDirectory StartupApp where
    workingDirectoryL =
        lens appWorkingDirectory $ \x y -> x { appWorkingDirectory = y }

instance HasSystem StartupApp where
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

    readFileBS path = do
        logDebug $ "readFileBS: " <> displayShow path
        appIO SystemError $ readFileBinary path

    writeFile path content = do
        logDebug $ "writeFile: " <> displayShow path
        appIO SystemError $ writeFileUtf8 path content

-- brittany-disable-next-binding

instance HasProcess StartupApp where
    callProcess cmd args = do
        -- N.B. this includes access tokens in log messages when used for
        -- git-clone. That's acceptable because:
        --
        -- - These tokens are ephemeral (5 minutes)
        -- - We generally accept secrets in DEBUG messages
        --
        logDebug $ "call: " <> fromString cmd <> " " <> displayShow args
        appIO SystemError $ Process.callProcess cmd args

    callProcessExitCode cmd args = do
        logDebug $ "call: " <> fromString cmd <> " " <> displayShow args
        ec <- appIO SystemError
            $ Process.withCreateProcess proc
            $ \_ _ _ p -> Process.waitForProcess p
        ec <$ logDebug ("exit code: " <> displayShow ec)
      where
        proc = (Process.proc cmd args) { Process.delegate_ctlc = True }

    readProcess cmd args stdin' = do
        logDebug $ "read: " <> fromString cmd <> " " <> displayShow args
        output <- appIO SystemError $ Process.readProcess cmd args stdin'
        output <$ logDebug ("output: " <> fromString output)

instance HasExit StartupApp where
    exitSuccess = do
        logDebug "exitSuccess"
        appIO SystemError Exit.exitSuccess

instance HasDownloadFile StartupApp where
    downloadFile url path = do
        logDebug $ "HTTP GET: " <> displayShow url <> " => " <> displayShow path
        appIO HttpError $ do
            request <- parseRequestThrow $ unpack url
            runResourceT $ httpSink request $ \_ -> sinkFile path

instance HasGitHub StartupApp where
    runGitHub req = do
        logDebug $ "GitHub request: " <> displayShow (displayGitHubRequest req)
        auth <- OAuth . encodeUtf8 . oAccessToken <$> view optionsL
        result <- liftIO $ do
            mgr <- getGlobalManager
            executeRequestWithMgr mgr auth req
        either (throwIO . GitHubError (displayGitHubRequest req)) pure result

appIO :: MonadUnliftIO m => (IOException -> AppError) -> IO a -> m a
appIO f = mapAppError f . liftIO

-- | Fully booted application environment
data App = App
    { appApp :: StartupApp
    , appConfig :: Config
    -- ^ Configuration loaded from @.restyled.yaml@
    , appPullRequest :: PullRequest
    -- ^ Original Pull Request being restyled
    , appRestyledPullRequest :: Maybe SimplePullRequest
    -- ^ Possible pre-existing Restyle Pull Request
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
    restyledPullRequestL =
        lens appRestyledPullRequest $ \x y -> x { appRestyledPullRequest = y }

instance HasSystem App where
    getCurrentDirectory = runApp getCurrentDirectory
    setCurrentDirectory = runApp . setCurrentDirectory
    doesFileExist = runApp . doesFileExist
    readFile = runApp . readFile
    readFileBS = runApp . readFileBS
    writeFile x = runApp . writeFile x

instance HasExit App where
    exitSuccess = runApp exitSuccess

instance HasProcess App where
    callProcess cmd = runApp . callProcess cmd
    callProcessExitCode cmd = runApp . callProcessExitCode cmd
    readProcess cmd args = runApp . readProcess cmd args

instance HasGit App where
    gitPush branch = callProcess "git" ["push", "origin", branch]
    gitPushForce branch =
        callProcess "git" ["push", "--force-with-lease", "origin", branch]
    gitMergeBase branch = do
        output <- readProcess "git" ["merge-base", branch, "HEAD"] ""
        pure $ listToMaybe $ lines output
    gitDiffNameOnly mRef = do
        let args = ["diff", "--name-only"] <> maybeToList mRef
        lines <$> readProcess "git" args ""
    gitCommitAll msg = do
        callProcess "git" ["commit", "-a", "--message", msg]
        dropWhileEnd isSpace <$> readProcess "git" ["rev-parse", "HEAD"] ""
    gitMerge branch = callProcess "git" ["merge", "--ff-only", branch]

instance HasDownloadFile App where
    downloadFile url = runApp . downloadFile url

instance HasGitHub App where
    runGitHub = runApp . runGitHub

appL :: Lens' App StartupApp
appL = lens appApp $ \x y -> x { appApp = y }

runApp :: RIO StartupApp a -> RIO App a
runApp = withRIO appApp

bootstrapApp :: MonadIO m => Options -> FilePath -> m App
bootstrapApp options path = runRIO app $ toApp <$> restylerSetup
  where
    app = StartupApp
        { appLogFunc = restylerLogFunc options
        , appOptions = options
        , appWorkingDirectory = path
        }

    toApp (pullRequest, mRestyledPullRequest, config) = App
        { appApp = app
        , appPullRequest = pullRequest
        , appRestyledPullRequest = mRestyledPullRequest
        , appConfig = config
        }
