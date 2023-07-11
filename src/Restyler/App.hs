module Restyler.App
  ( AppT
  , runAppT
  , App (..)
  , StartupApp (..)
  , bootstrapApp

    -- * App's implementation, exposed for use outside of AppT
  , GitHubError (..)
  , runGitHubInternal
  ) where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import qualified Data.Text as T
import GitHub.Auth
import qualified GitHub.Data.Definitions as GitHub
import GitHub.Request
import GitHub.Request.Display
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (Request)
import qualified Relude as Prelude
import Restyler.App.Class
import Restyler.Config
import Restyler.Git
import Restyler.Options
import Restyler.PullRequest
import Restyler.Setup
import Restyler.Statsd (HasStatsClient (..), StatsClient)
import qualified System.Directory as Directory
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
    , MonadCatch
    , MonadMask
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    , MonadReader app
    )

instance MonadUnliftIO m => MonadSystem (AppT app m) where
  getCurrentDirectory = do
    logDebug "getCurrentDirectory"
    liftIO Directory.getCurrentDirectory

  setCurrentDirectory path = do
    logDebug $ "setCurrentDirectory" :# ["path" .= path]
    liftIO $ Directory.setCurrentDirectory path

  doesFileExist path = do
    logDebug $ "doesFileExist" :# ["path" .= path]
    liftIO $ Directory.doesFileExist path

  doesDirectoryExist path = do
    logDebug $ "doesDirectoryExist" :# ["path" .= path]
    liftIO $ Directory.doesDirectoryExist path

  isFileExecutable path = do
    logDebug $ "isFileExecutable" :# ["path" .= path]
    liftIO $ Directory.executable <$> Directory.getPermissions path

  isFileSymbolicLink path = do
    logDebug $ "isFileSymbolicLink" :# ["path" .= path]
    liftIO $ Directory.pathIsSymbolicLink path

  listDirectory path = do
    logDebug $ "listDirectory" :# ["path" .= path]
    liftIO $ Directory.listDirectory path

  readFileBS path = do
    logDebug $ "readFileBS" :# ["path" .= path]
    liftIO $ Prelude.readFileBS path

  writeFile path content = do
    logDebug $ "writeFile" :# ["path" .= path]
    liftIO $ Prelude.writeFile path $ unpack content

  removeFile path = do
    logDebug $ "removeFile" :# ["path" .= path]
    liftIO $ Directory.removeFile path

instance MonadUnliftIO m => MonadProcess (AppT app m) where
  callProcess cmd args = do
    -- N.B. this includes access tokens in log messages when used for
    -- git-clone. That's acceptable because:
    --
    -- - These tokens are ephemeral (5 minutes)
    -- - We generally accept secrets in DEBUG messages
    --
    logDebug $ "callProcess" :# ["command" .= cmd, "arguments" .= args]
    liftIO $ Process.callProcess cmd args

  callProcessExitCode cmd args = do
    logDebug
      $ "callProcessExitCode"
      :# ["command" .= cmd, "arguments" .= args]
    ec <- liftIO $ Process.withCreateProcess proc $ \_ _ _ p ->
      Process.waitForProcess p
    (if ec == ExitSuccess then logDebug else logWarn)
      $ "callProcessExitCode"
      :# [ "command" .= cmd
         , "arguments" .= args
         , "exitCode" .= exitCodeInt ec
         ]
    pure ec
   where
    proc = (Process.proc cmd args) {Process.delegate_ctlc = True}

  readProcess cmd args = do
    logDebug
      $ "readProcess"
      :# ["command" .= cmd, "arguments" .= args]
    output <- liftIO $ Process.readProcess cmd args ""
    logDebug
      $ "readProcess"
      :# [ "command" .= cmd
         , "arguments" .= args
         , "output" .= output
         ]
    pure output

  readProcessExitCode cmd args = do
    logDebug
      $ "readProcess"
      :# ["command" .= cmd, "arguments" .= args]
    (ec, output, err) <- liftIO $ Process.readProcessWithExitCode cmd args ""
    (if ec == ExitSuccess then logDebug else logWarn)
      $ "readProcessExitCode"
      :# [ "command" .= cmd
         , "arguments" .= args
         , "output" .= output
         , "errorOutput" .= err
         ]
    pure (ec, output)

instance MonadUnliftIO m => MonadExit (AppT app m) where
  exitSuccess = do
    logDebug "exitSuccess"
    liftIO Exit.exitSuccess

instance MonadUnliftIO m => MonadDownloadFile (AppT app m) where
  downloadFile url path = do
    liftIO $ do
      request <- parseRequestThrow $ unpack url
      runResourceT $ httpSink request $ \_ -> sinkFile path

data GitHubError = GitHubError
  { gheRequest :: DisplayGitHubRequest
  , gheError :: GitHub.Error
  }
  deriving stock (Show)

instance Exception GitHubError where
  displayException GitHubError {..} =
    "Error communication with GitHub:"
      <> "\n  Request:"
      <> show @String gheRequest
      <> "\n  Exception:"
      <> show @String gheError

instance (MonadUnliftIO m, HasOptions app) => MonadGitHub (AppT app m) where
  runGitHub = runGitHubInternal

runGitHubInternal
  :: (MonadIO n, MonadLogger n, MonadReader env n, HasOptions env)
  => ParseResponse m a
  => GenRequest m k a
  -> n a
runGitHubInternal req = do
  logDebug
    $ "runGitHub"
    :# ["request" .= show @Text (displayGitHubRequest req)]
  auth <- OAuth . encodeUtf8 . oAccessToken <$> view optionsL
  result <- liftIO $ do
    mgr <- getGlobalManager
    executeRequestWithMgr mgr auth req
  either (throwIO . GitHubError (displayGitHubRequest req)) pure result

runAppT :: MonadUnliftIO m => HasLogger app => app -> AppT app m a -> m a
runAppT app f = runLoggerLoggingT app $ runReaderT (unAppT f) app

data StartupApp = StartupApp
  { appLogger :: Logger
  , appOptions :: Options
  , appWorkingDirectory :: FilePath
  , appStatsClient :: StatsClient
  }

instance HasLogger StartupApp where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

instance HasOptions StartupApp where
  optionsL = lens appOptions $ \x y -> x {appOptions = y}

instance HasWorkingDirectory StartupApp where
  workingDirectoryL =
    lens appWorkingDirectory $ \x y -> x {appWorkingDirectory = y}

instance HasStatsClient StartupApp where
  statsClientL = lens appStatsClient $ \x y -> x {appStatsClient = y}

data App = App
  { appApp :: StartupApp
  , appConfig :: Config
  , appPullRequest :: PullRequest
  }

appL :: Lens' App StartupApp
appL = lens appApp $ \x y -> x {appApp = y}

instance HasLogger App where
  loggerL = appL . loggerL

instance HasOptions App where
  optionsL = appL . optionsL

instance HasWorkingDirectory App where
  workingDirectoryL = appL . workingDirectoryL

instance HasConfig App where
  configL = lens appConfig $ \x y -> x {appConfig = y}

instance HasPullRequest App where
  pullRequestL = lens appPullRequest $ \x y -> x {appPullRequest = y}

instance MonadUnliftIO m => MonadGit (AppT App m) where
  gitPush branch = callProcess "git" ["push", "origin", branch]
  gitPushForce branch =
    callProcess "git" ["push", "--force", "origin", branch]
  gitDiffNameOnly mRef = do
    let args = ["diff", "--name-only"] <> maybeToList mRef
    map unpack . lines . pack <$> readProcess "git" args
  gitFormatPatch mRef = do
    let args = ["format-patch", "--stdout"] <> maybeToList mRef
    pack <$> readProcess "git" args
  gitCommitAll msg = do
    callProcess "git" ["commit", "-a", "--message", msg]
    unpack
      . T.dropWhileEnd isSpace
      . pack
      <$> readProcess
        "git"
        ["rev-parse", "HEAD"]
  gitCheckout branch = do
    callProcess "git" ["checkout", "--no-progress", "-b", branch]

bootstrapApp
  :: MonadUnliftIO m
  => Options
  -> Logger
  -> FilePath
  -> StatsClient
  -> m App
bootstrapApp options logger path statsClient =
  runAppT app $ toApp <$> restylerSetup
 where
  app =
    StartupApp
      { appLogger = logger
      , appOptions = options
      , appWorkingDirectory = path
      , appStatsClient = statsClient
      }
  toApp (pullRequest, config) =
    App {appApp = app, appPullRequest = pullRequest, appConfig = config}
