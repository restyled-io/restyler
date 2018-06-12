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
    )
where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import GitHub.Endpoints.Issues.Comments
import GitHub.Endpoints.PullRequests
import GitHub.Endpoints.Repos.Statuses
import GitHub.Request
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (Request)
import Restyler.Capabilities.Docker
import Restyler.Capabilities.Git
import Restyler.Capabilities.GitHub
import Restyler.Capabilities.RemoteFile
import Restyler.Capabilities.System
import Restyler.Model.Config
import Restyler.Model.RemoteFile
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.Process

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
    | GitError IOException
    -- ^ Error running a @git@ operation
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

-- | Run an @'IO'@ computation and capture @'IOException'@s to the given type
appIO :: MonadIO m => (IOException -> AppError) -> IO a -> AppT m a
appIO err f = AppT $ do
    result <- liftIO $ tryIO f
    either (throwError . err) pure result

instance MonadIO m => MonadGit (AppT m) where
    cloneRepository url dir = do
        logDebugN $ "git clone " <> tshow [masked, dir]
        appIO GitError $ callProcess "git" ["clone", unpack url, dir]
      where
        masked = T.unpack $ scheme <> "://<creds>" <> T.dropWhile (/= '@') rest
        (scheme, rest) = T.breakOn "://" url

    checkoutBranch b branch = do
        logDebugN $ "git checkout " <> branch
        appIO GitError $ callProcess "git" $ ["checkout"] ++ [ "-b" | b ] ++ [unpack branch]

    changedPaths branch = do
        logDebugN $ "git diff --name-only " <> branch
        appIO GitError $ lines <$> readProcess "git" ["diff", "--name-only", unpack branch] ""

    commitAll msg = do
        logDebugN "git commit"
        appIO GitError $ callProcess "git" ["commit", "-am", unpack msg]

    fetchOrigin remoteRef localRef = do
        logDebugN $ "git fetch origin " <> remoteRef <> ":" <> localRef
        appIO GitError $ callProcess "git" ["fetch", "origin", unpack $ remoteRef <> ":" <> localRef]

    pushOrigin branch = do
        logDebugN $ "git push origin " <> branch
        appIO GitError $ callProcess "git" ["push", "origin", unpack branch]

    forcePushOrigin branch = do
        logDebugN $ "git push origin " <> branch
        appIO GitError $ callProcess "git" ["push", "--force-with-lease", "origin", unpack branch]

instance MonadIO m => MonadGitHub (AppT m) where
    getPullRequest owner name num = runGitHub $ pullRequestR owner name num

    findPullRequest owner name base head = do
        results <- runGitHub $ pullRequestsForR owner name
            (optionsBase base <> optionsHead head) FetchAll
        pure $ results V.!? 0

    createPullRequest owner name create =
        runGitHub $ createPullRequestR owner name create

    updatePullRequest owner name id' edit =
        runGitHub $ updatePullRequestR owner name id' edit

    getComments owner name id' = runGitHub $ commentsR owner name id' FetchAll

    createComment owner name id' body = runGitHub_ $ createCommentR
        owner
        name
        id'
        body

    deleteComment owner name id' = runGitHub_ $ deleteCommentR owner name id'

    createStatus owner name sha status = runGitHub_ $ createStatusR owner name sha status

instance MonadIO m => MonadSystem (AppT m) where
    getCurrentDirectory = do
        logDebugN "getCurrentDirectory"
        appIO SystemError Directory.getCurrentDirectory

    doesFileExist path = do
        logDebugN $ "doesFileExist: " <> tshow path
        appIO SystemError $ Directory.doesFileExist path

    setCurrentDirectory path = do
        logDebugN $ "setCurrentDirectory: " <> tshow path
        appIO SystemError $ Directory.setCurrentDirectory path

    readFile path = do
        logDebugN $ "readFile: " <> tshow path
        appIO SystemError $ T.readFile path

    exitSuccess = appIO SystemError Exit.exitSuccess

instance MonadIO m => MonadDocker (AppT m) where
    dockerRun args = do
        logDebugN $ "docker run " <> tshow args
        appIO DockerError $ callProcess "docker" $ "run" : args

instance MonadIO m => MonadRemoteFile (AppT m) where
    fetchRemoteFile RemoteFile{..} = do
        let url = getUrl rfUrl
        logInfoN $ "Fetching " <> tshow rfPath <> " from " <> tshow url
        appIO RemoteFileError $ do
            request <- parseRequest $ unpack url
            runResourceT $ httpSink request $ \_ -> sinkFile rfPath

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
