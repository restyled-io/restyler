{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.App
    (
    -- * Application environment
      App(..)
    , AppT
    , runAppT

    -- * Application errors
    , AppError(..)
    , mapAppError

    -- * GitHub
    , runGitHub
    , runGitHub_

    -- ** Re-exports
    , module GitHub.Endpoints.Issues.Comments
    , module GitHub.Endpoints.PullRequests
    , module GitHub.Endpoints.Repos.Statuses

    -- * System
    -- ** Directories
    , getCurrentDirectory
    , setCurrentDirectory

    -- ** Files
    , doesFileExist
    , readFile

    -- ** Environment
    , exitSuccess

    -- ** Processes
    , callProcess
    , readProcess

    -- * HTTP
    , fetchRemoteFile
    )
where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GitHub.Endpoints.Issues.Comments hiding (comment, comments)
import GitHub.Endpoints.PullRequests hiding (pullRequest)
import GitHub.Endpoints.Repos.Statuses
import GitHub.Request
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (Request)
import Restyler.Model.Config
import Restyler.Model.RemoteFile
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.Process as Process

-- | Application environment
data App = App
    { appLogLevel :: LogLevel
    , appLogColor :: Bool
    , appAccessToken :: Text
    , appPullRequest :: PullRequest
    -- ^ The @'PullRequest'@ we are restyling
    , appConfig :: Config
    -- ^ Configuration loaded from @.restyled.yaml@
    , appRestyledPullRequest :: Maybe SimplePullRequest
    -- ^ Existing restyled @'PullRequest'@ if it exists
    }

-- | All possible application error conditions
data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigurationError String
    -- ^ We couldn't load a @.restyled.yaml@
    | DockerError IOException
    -- ^ Error running a @docker@ operation
    | GitHubError Error
    -- ^ We encountered a GitHub API error during restyling
    | SystemError IOException
    -- ^ Trouble reading a file or etc
    | RemoteFileError IOException
    -- ^ Trouble performing some HTTP request
    | OtherError IOException
    -- ^ A minor escape hatch for @'IOException'@s
    deriving Show

-- | Run a computation, and modify any thrown @'AppError'@s
mapAppError :: MonadError AppError m => (AppError -> AppError) -> m a -> m a
mapAppError f = (`catchError` throwError . f)

newtype AppT m a = AppT
    { runAppT :: ReaderT App (LoggingT (ExceptT AppError m)) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError AppError
        , MonadReader App
        , MonadLogger
        )


-- | Run a GitHub @'Request'@
runGitHub :: MonadIO m => Request k a -> AppT m a
runGitHub req = do
    logDebugN $ "GitHub request: " <> showGitHubRequest req
    auth <- asks $ OAuth . encodeUtf8 . appAccessToken
    result <- appIO OtherError $ do
        mgr <- getGlobalManager
        executeRequestWithMgr mgr auth req

    either (throwError . GitHubError) pure result

-- | @'runGitHub'@ but discard the result
runGitHub_ :: MonadIO m => Request k a -> AppT m ()
runGitHub_ = void . runGitHub

-- | Show a GitHub @'Request'@, useful for debugging
-- brittany-disable-next-binding
showGitHubRequest :: Request k a -> Text
showGitHubRequest (SimpleQuery (Query ps qs)) = mconcat
    [ "[GET] "
    , "/" <> T.intercalate "/" ps
    , "?" <> T.intercalate "&" (queryParts qs)
    ]
showGitHubRequest (SimpleQuery (PagedQuery ps qs fc)) = mconcat
    [ "[GET] "
    , "/" <> T.intercalate "/" ps
    , "?" <> T.intercalate "&" (queryParts qs)
    , " (" <> tshow fc <> ")"
    ]
showGitHubRequest (SimpleQuery (Command m ps _body)) = mconcat
    [ "[" <> T.toUpper (tshow m) <> "] "
    , "/" <> T.intercalate "/" ps
    ]
showGitHubRequest (StatusQuery _ _) = "<status query>"
showGitHubRequest (HeaderQuery _ _) = "<header query>"
showGitHubRequest (RedirectQuery _) = "<redirect query>"

queryParts :: QueryString -> [Text]
queryParts = map $ \(k, mv) -> decodeUtf8 k <> "=" <> maybe "" decodeUtf8 mv

getCurrentDirectory :: MonadIO m => AppT m FilePath
getCurrentDirectory = do
    logDebugN "getCurrentDirectory"
    appIO SystemError Directory.getCurrentDirectory

doesFileExist :: MonadIO m => FilePath -> AppT m Bool
doesFileExist path = do
    logDebugN $ "doesFileExist: " <> tshow path
    appIO SystemError $ Directory.doesFileExist path

setCurrentDirectory :: MonadIO m => FilePath -> AppT m ()
setCurrentDirectory path = do
    logDebugN $ "setCurrentDirectory: " <> tshow path
    appIO SystemError $ Directory.setCurrentDirectory path

readFile :: MonadIO m => FilePath -> AppT m Text
readFile path = do
    logDebugN $ "readFile: " <> tshow path
    appIO SystemError $ T.readFile path

exitSuccess :: MonadIO m => AppT m ()
exitSuccess = do
    logDebugN "exitSuccess"
    appIO SystemError Exit.exitSuccess

fetchRemoteFile :: MonadIO m => RemoteFile -> AppT m ()
fetchRemoteFile RemoteFile {..} = do
    let url = getUrl rfUrl
    logInfoN $ "Fetching " <> tshow rfPath <> " from " <> tshow url
    appIO RemoteFileError $ do
        request <- parseRequest $ unpack url
        runResourceT $ httpSink request $ \_ -> sinkFile rfPath

callProcess :: MonadIO m => String -> [String] -> AppT m ()
callProcess cmd args = do
    -- N.B. this injects access tokens into the logs when calling git-clone, but
    -- that's OK because it's DEBUG and they're short-lived anyway.
    logDebugN $ pack $ "call: " <> cmd <> " " <> show args
    appIO SystemError $ Process.callProcess cmd args

readProcess :: MonadIO m => String -> [String] -> String -> AppT m String
readProcess cmd args stdin = do
    logDebugN $ pack $ "read: " <> cmd <> " " <> show args
    output <- appIO SystemError $ Process.readProcess cmd args stdin
    output <$ logDebugN ("output: " <> pack output)

-- | Run an @'IO'@ computation and capture @'IOException'@s to the given type
appIO :: MonadIO m => (IOException -> AppError) -> IO a -> AppT m a
appIO err f = AppT $ do
    result <- liftIO $ tryIO f
    either (throwError . err) pure result
