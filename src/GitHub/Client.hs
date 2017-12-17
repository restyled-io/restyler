{-# LANGUAGE DataKinds #-}
-- |
--
-- Custom Monad for running GitHub API actions, and lifted versions of the API
-- actions required in this codebase. See @"GitHub.Endpoints.Installations"@ for
-- getting a token using GitHub Apps authentication.
--
module GitHub.Client
    ( GitHubRW
    , runGitHub
    , runGitHubThrow
    , createComment
    , createPullRequest
    , getPullRequest
    ) where

import Control.Exception (throwIO)
import Control.Monad.Except
import Control.Monad.Operational
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GitHub
import GitHub.Endpoints.Issues.Comments (createCommentR)
import GitHub.Endpoints.PullRequests (createPullRequestR, pullRequestR)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Monad for running Read/Write GitHub API actions
type GitHubRW = Program (Request 'RW)

-- | Run GitHub API actions and return an error or the result
runGitHub :: MonadIO m => Text -> GitHubRW a -> m (Either Error a)
runGitHub token m = runExceptT $ do
    mgr <- liftIO $ newManager tlsManagerSettings
    go mgr m
  where
    go mgr m' = case view m' of
        Return a -> return a
        req :>>= k -> do
            b <- ExceptT $ liftIO $ executeRequestWithMgr mgr auth req
            go mgr (k b)

    auth = OAuth $ encodeUtf8 token

-- | A version that throws (shown) errors in @'IO'@
--
-- Actually most useful in a CLI application with overall IO error-handling.
--
runGitHubThrow :: Text -> GitHubRW a -> IO a
runGitHubThrow token m = do
    result <- runGitHub token m
    either (throwIO . userError . ("GitHub Error: " ++) . show) pure result

-- | @'pullRequestR'@ lifted to @'GitHubRW'@
--
-- Renamed to reduce clashing.
--
getPullRequest :: Name Owner -> Name Repo -> Id PullRequest -> GitHubRW PullRequest
getPullRequest o r = singleton . pullRequestR o r

-- | @'createPullRequestR'@ lifted to @'GitHubRW'@
createPullRequest :: Name Owner -> Name Repo -> CreatePullRequest -> GitHubRW PullRequest
createPullRequest o r = singleton . createPullRequestR o r

-- | @'createCommentR'@ lifted to @'GitHubRW'@
createComment :: Name Owner -> Name Repo -> Id Issue -> Text -> GitHubRW Comment
createComment o r i = singleton . createCommentR o r i
