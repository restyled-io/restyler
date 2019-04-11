{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Restyler.App.Class
    ( MonadApp(..)

    -- * Re-exports
    -- |
    --
    -- This is useful because it defines what GitHub APIs we use and takes care
    -- of the hidings we'd otherwise need to do at every import site.
    --
    , module GitHub.Endpoints.Issues.Comments
    , module GitHub.Endpoints.Issues.Labels
    , module GitHub.Endpoints.PullRequests
    , module GitHub.Endpoints.PullRequests.ReviewRequests
    , module GitHub.Endpoints.Repos.Statuses
    ) where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import GitHub.Endpoints.Issues.Comments hiding (comment, comments)
import GitHub.Endpoints.Issues.Labels
import GitHub.Endpoints.PullRequests hiding (pullRequest)
import GitHub.Endpoints.PullRequests.ReviewRequests
import GitHub.Endpoints.Repos.Statuses
import GitHub.Request
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (Request)
import Restyler.App.Type
import qualified RIO.Directory as Directory
import qualified RIO.Text as T
import qualified RIO.Vector as V
import qualified System.Exit as Exit
import qualified System.Process as Process

class
    ( Monad m
    , MonadLogger m
    , MonadReader App m
    , MonadError AppError m
    )
    => MonadApp m where
        runGitHub :: Request k a -> m a

        -- | Fetch the first page using @'runGitHub'@, return the first item
        runGitHubFirst :: (FetchCount -> Request k (Vector a)) -> m (Maybe a)
        runGitHubFirst f = (V.!? 0) <$> runGitHub (f 1)

        -- | @'void' . 'runGitHub'@
        runGitHub_ :: Request k a -> m ()
        runGitHub_ = void . runGitHub

        getCurrentDirectory :: m FilePath
        setCurrentDirectory :: FilePath -> m ()

        doesFileExist :: FilePath -> m Bool
        readFile :: FilePath -> m Text

        exitSuccess :: m ()

        callProcess :: String -> [String] -> m ()
        readProcess :: String -> [String] -> String -> m String

        downloadFile :: Text -> FilePath -> m ()

instance MonadIO m => MonadApp (AppT m) where
    runGitHub req = do
        logDebugN $ "GitHub request: " <> showGitHubRequest req
        auth <- asks $ OAuth . encodeUtf8 . appAccessToken
        result <- appIO OtherError $ do
            mgr <- getGlobalManager
            executeRequestWithMgr mgr auth req
        either (throwError . GitHubError) pure result

    getCurrentDirectory = do
        logDebugN "getCurrentDirectory"
        appIO SystemError Directory.getCurrentDirectory

    setCurrentDirectory path = do
        logDebugN $ "setCurrentDirectory: " <> tshow path
        appIO SystemError $ Directory.setCurrentDirectory path

    doesFileExist path = do
        logDebugN $ "doesFileExist: " <> tshow path
        appIO SystemError $ Directory.doesFileExist path

    readFile path = do
        logDebugN $ "readFile: " <> tshow path
        appIO SystemError $ readFileUtf8 path

    exitSuccess = do
        logDebugN "exitSuccess"
        appIO SystemError Exit.exitSuccess

    callProcess cmd args = do
        -- N.B. this includes access tokens in log messages when used for
        -- git-clone. That's acceptable because:
        --
        -- - These tokens are ephemeral (5 minutes)
        -- - We generally accept secrets in DEBUG messages
        --
        logDebugN $ pack $ "call: " <> cmd <> " " <> show args
        appIO SystemError $ Process.callProcess cmd args

    readProcess cmd args stdin' = do
        logDebugN $ pack $ "read: " <> cmd <> " " <> show args
        output <- appIO SystemError $ Process.readProcess cmd args stdin'
        output <$ logDebugN ("output: " <> pack output)

    downloadFile url path = do
        logDebugN $ "HTTP GET: " <> tshow url <> " => " <> tshow path
        appIO HttpError $ do
            request <- parseRequest $ unpack url
            runResourceT $ httpSink request $ \_ -> sinkFile path

-- | Run an @'IO'@ computation and capture @'IOException'@s to the given type
appIO :: MonadIO m => (IOException -> AppError) -> IO a -> AppT m a
appIO err f = AppT $ do
    result <- liftIO $ tryIO f
    either (throwError . err) pure result

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
