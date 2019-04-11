module GitHub.Request.Display
    ( displayGitHubRequest
    )
where

import RIO

import qualified RIO.Text as T
import GitHub.Data

newtype DisplayGitHubRequest k a
    = DisplayGitHubRequest (Request k a)

instance Show (DisplayGitHubRequest k a) where
    show (DisplayGitHubRequest req) = T.unpack $ format req
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
        queryParts = map $ \(k, mv) ->
            T.decodeUtf8With T.lenientDecode k
                <> "="
                <> maybe "" (T.decodeUtf8With T.lenientDecode) mv

displayGitHubRequest :: Request k a -> Utf8Builder
displayGitHubRequest = displayShow . DisplayGitHubRequest
