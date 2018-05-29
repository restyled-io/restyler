{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Restyler.GitHub
    ( runGitHub
    , runGitHub_
    , showGitHubRequest
    , showGitHubError
    )
where

import Restyler.Prelude

import qualified Data.Text as T
import GitHub.Data
import GitHub.Request
import Network.HTTP.Client.TLS

runGitHub :: Request k a -> AppM a
runGitHub req = do
    logDebugN $ "GitHub request: " <> showGitHubRequest req
    auth <- asks $ OAuth . encodeUtf8 . appAccessToken
    result <- liftIOApp $ do
        mgr <- getGlobalManager
        executeRequestWithMgr mgr auth req

    either (throwError . GitHubError) pure result

runGitHub_ :: Request k a -> AppM ()
runGitHub_ = void . runGitHub

-- | Show a GitHub @'Request'@, useful for debugging
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
showGitHubRequest (SimpleQuery (Command m ps _body)) =
    mconcat ["[" <> T.toUpper (tshow m) <> "] ", "/" <> T.intercalate "/" ps]
showGitHubRequest (StatusQuery _ _) = "<status query>"
showGitHubRequest (HeaderQuery _ _) = "<header query>"
showGitHubRequest (RedirectQuery _) = "<redirect query>"

-- | Show a GitHub @'Error'@
--
-- This relies on @'HttpException'@ not leaking anything sensitive for our
-- use-cases, which is true for now (e.g. it masks @Authorization@ headers).
--
showGitHubError :: Error -> String
showGitHubError (HTTPError e) = "HTTP exception: " <> show e
showGitHubError (ParseError e) = "Unable to parse response: " <> unpack e
showGitHubError (JsonError e) = "Malformed response: " <> unpack e
showGitHubError (UserError e) = "User error: " <> unpack e

queryParts :: QueryString -> [Text]
queryParts = map $ \(k, mv) -> decodeUtf8 k <> "=" <> maybe "" decodeUtf8 mv
