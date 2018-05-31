{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Text as T
import qualified Data.Text.IO as T
import GitHub.Endpoints.Issues.Comments
import GitHub.Endpoints.PullRequests
import GitHub.Endpoints.Repos.Statuses
import GitHub.Request
import Network.HTTP.Client.TLS
import Restyler.Capabilities.Docker
import Restyler.Capabilities.Git
import Restyler.Capabilities.GitHub
import Restyler.Capabilities.System
import Restyler.Model.Config
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.Process as Process

-- | Application environment
data App = App
    { appLogLevel :: LogLevel
    , appAccessToken :: Text
    , appPullRequest :: PullRequest
    -- ^ The @'PullRequest'@ we are restyling
    , appConfig :: Config
    -- ^ Configuration loaded from @.restyled.yaml@
    }

-- | All possible application error conditions
data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigurationError String
    -- ^ We couldn't load a @.restyled.yaml@
    | GitHubError Error
    -- ^ We encountered a GitHub API error during restyling
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

-- | Ensures all @'IOException'@s are handled in the @'MonadError'@ context
instance MonadIO m => MonadIO (AppT m) where
    liftIO f = AppT $ do
        result <- liftIO $ tryIO f
        either (throwError . OtherError) pure result

instance MonadIO m => MonadGit (AppT m) where
    cloneRepository url dir = do
        -- N.B. Re-implements @'Restyler.Process.callProcss'@ to avoid logging the
        -- access-token present in the clone URL.
        logDebugN $ "callProcess: " <> tshow ["git", "clone", masked, dir]
        liftIO $ Process.callProcess "git" ["clone", unpack url, dir]
      where
        masked = T.unpack $ scheme <> "://<creds>" <> T.dropWhile (/= '@') rest
        (scheme, rest) = T.breakOn "://" url

    checkoutBranch b branch =
        callProcess "git" $ ["checkout"] ++ [ "-b" | b ] ++ [unpack branch]

    changedPaths branch =
        lines <$> readProcess "git" ["diff", "--name-only", unpack branch] ""

    commitAll msg = callProcess "git" ["commit", "-am", unpack msg]

    fetchOrigin remoteRef localRef =
        callProcess "git" ["fetch", "origin", unpack $ remoteRef <> ":" <> localRef]

    pushOrigin branch = callProcess "git" ["push", "origin", unpack branch]

    forcePushOrigin branch =
        callProcess "git" ["push", "--force-with-lease", "origin", unpack branch]

    branchHeadMessage branch = strip . pack <$$> readProcessMay
        "git"
        ["log", "-n", "1", "--format=%B", unpack branch]
        ""

instance MonadIO m => MonadGitHub (AppT m) where
    getPullRequest owner name num = runGitHub $ pullRequestR owner name num

    createPullRequest owner name create =
        runGitHub $ createPullRequestR owner name create

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
        liftIO Directory.getCurrentDirectory

    doesFileExist path = do
        logDebugN $ "doesFileExist: " <> tshow path
        liftIO $ Directory.doesFileExist path

    setCurrentDirectory path = do
        logDebugN "setCurrentDirectory"
        liftIO $ Directory.setCurrentDirectory path

    readFile path = do
        logDebugN "readFile"
        liftIO $ T.readFile path

    exitSuccess = liftIO Exit.exitSuccess

instance MonadIO m => MonadDocker (AppT m) where
    dockerRun = callProcess "docker" . ("run" :)

callProcess :: MonadIO m => String -> [String] -> AppT m ()
callProcess cmd args = do
    logDebugN $ "callProcess: " <> tshow (cmd : args)
    liftIO $ Process.callProcess cmd args

readProcess :: MonadIO m => String -> [String] -> String -> AppT m String
readProcess cmd args stdin = do
    logDebugN $ "readProcess: " <> tshow (cmd : args)
    liftIO $ Process.readProcess cmd args stdin

readProcessMay
    :: MonadIO m => String -> [String] -> String -> AppT m (Maybe String)
readProcessMay cmd args = hushM . readProcess cmd args

-- | Run a GitHub @'Request'@
runGitHub :: MonadIO m => Request k a -> AppT m a
runGitHub req = do
    logDebugN $ "GitHub request: " <> showGitHubRequest req
    auth <- asks $ OAuth . encodeUtf8 . appAccessToken
    result <- liftIO $ do
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
