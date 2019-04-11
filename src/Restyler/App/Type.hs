{-# LANGUAGE FlexibleContexts #-}

module Restyler.App.Type
    ( App(..)

    -- * Application errors
    , AppError(..)
    , mapAppError
    ) where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import qualified Data.Yaml as Yaml
import GitHub.Request
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (Request)
import Restyler.App.Class
import Restyler.Config
import Restyler.Options
import qualified RIO.Directory as Directory
import qualified RIO.Text as T
import qualified System.Exit as Exit
import qualified System.Process as Process

-- | Application environment
data App = App
    { appLogFunc :: LogFunc
    , appAccessToken :: Text
    , appPullRequest :: PullRequest
    -- ^ The @'PullRequest'@ we are restyling
    , appRestyledPullRequest :: Maybe SimplePullRequest
    -- ^ Existing restyled @'PullRequest'@ if it exists
    , appConfig :: Config
    -- ^ Configuration loaded from @.restyled.yaml@
    , appOptions :: Options
    -- ^ Original command-line options
    , appWorkingDirectory :: FilePath
    -- ^ Temporary directory we are working in
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasOptions App where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasWorkingDirectory App where
    workingDirectoryL = lens appWorkingDirectory $ \x y ->
        x { appWorkingDirectory = y }

instance HasConfig App where
    configL = lens appConfig $ \x y -> x { appConfig = y }

instance HasPullRequest App where
    pullRequestL = lens appPullRequest $ \x y -> x { appPullRequest = y }

instance HasRestyledPullRequest App where
    restyledPullRequestL = lens appRestyledPullRequest $ \x y ->
        x { appRestyledPullRequest = y }

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
mapAppError
    :: (MonadUnliftIO m, Exception e)
    => (e -> AppError) -> m a -> m a
mapAppError f = (`catch` throwIO . f)

-- | Run an @'IO'@ computation and capture @'IOException'@s to the given type
appIO :: MonadUnliftIO m => (IOException -> AppError) -> IO a -> m a
appIO f = mapAppError f . liftIO

instance HasSystem App where
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

instance HasExit App where
    exitSuccess = do
        logDebug "exitSuccess"
        appIO SystemError Exit.exitSuccess

instance HasProcess App where
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

instance HasDownloadFile App where
    downloadFile url path = do
        logDebug $ "HTTP GET: " <> displayShow url <> " => " <> displayShow path
        appIO HttpError $ do
            request <- parseRequest $ unpack url
            runResourceT $ httpSink request $ \_ -> sinkFile path

instance HasGitHub App where
    runGitHub req = do
        logDebug $ "GitHub request: " <> showGitHubRequest req
        auth <- asks $ OAuth . encodeUtf8 . appAccessToken
        result <- appIO OtherError $ do
            mgr <- getGlobalManager
            executeRequestWithMgr mgr auth req
        either (throwIO . GitHubError) pure result

-- brittany-disable-next-binding

-- | Show a GitHub @'Request'@, useful for debugging
--
-- TODO: Use a newtype and @'displayShow'@
--
showGitHubRequest :: Request k a -> Utf8Builder
showGitHubRequest = fromString . unpack . format
  where
    format :: Request k a -> Text
    format = \case
        SimpleQuery (Query ps qs) -> mconcat
            [ "[GET] "
            , "/" <> T.intercalate "/" ps
            , "?" <> T.intercalate "&" (queryParts qs)
            ]
        SimpleQuery (PagedQuery ps qs fc) -> mconcat
            [ "[GET] "
            , "/" <> T.intercalate "/" ps
            , "?" <> T.intercalate "&" (queryParts qs)
            , " (" <> tshow fc <> ")"
            ]
        SimpleQuery (Command m ps _body) -> mconcat
            [ "[" <> T.toUpper (tshow m) <> "] "
            , "/" <> T.intercalate "/" ps
            ]
        StatusQuery _ _ -> "<status query>"
        HeaderQuery _ _ -> "<header query>"
        RedirectQuery _ -> "<redirect query>"

    queryParts :: QueryString -> [Text]
    queryParts = map $ \(k, mv) -> decodeUtf8 k <> "=" <> maybe "" decodeUtf8 mv
