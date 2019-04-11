{-# LANGUAGE LambdaCase #-}

module GitHub.Request.Display
    ( DisplayGitHubRequest(..)
    )
where

import Prelude

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GitHub.Data

newtype DisplayGitHubRequest k a
    = DisplayGitHubRequest (Request k a)

instance Show (DisplayGitHubRequest k a) where
    show (DisplayGitHubRequest req) = unpack $ formatRequest req

formatRequest :: Request k a -> Text
formatRequest = \case
    SimpleQuery (Query ps qs) -> mconcat
        [ "[GET] "
        , "/" <> T.intercalate "/" ps
        , "?" <> T.intercalate "&" (queryParts qs)
        ]
    SimpleQuery (PagedQuery ps qs fc) -> mconcat
        [ "[GET] "
        , "/" <> T.intercalate "/" ps
        , "?" <> T.intercalate "&" (queryParts qs)
        , " (" <> pack (show fc) <> ")"
        ]
    SimpleQuery (Command m ps _body) ->
        mconcat
            [ "[" <> T.toUpper (pack $ show m) <> "] "
            , "/" <> T.intercalate "/" ps
            ]
    StatusQuery _ _ -> "<status query>"
    HeaderQuery _ _ -> "<header query>"
    RedirectQuery _ -> "<redirect query>"

queryParts :: QueryString -> [Text]
queryParts = map $ \(k, mv) -> decodeUtf8 k <> "=" <> maybe "" decodeUtf8 mv
