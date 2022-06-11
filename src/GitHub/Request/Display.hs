module GitHub.Request.Display
    ( DisplayGitHubRequest
    , displayGitHubRequest
    ) where

import Prelude

import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GitHub.Request

newtype DisplayGitHubRequest = DisplayGitHubRequest
    { _unDisplayGitHubRequest :: Text
    }
    deriving newtype (Eq, Show)

displayGitHubRequest :: GenRequest m k a -> DisplayGitHubRequest
displayGitHubRequest = DisplayGitHubRequest . \case
    Query ps qs -> mconcat
        [ "[GET] "
        , "/" <> T.intercalate "/" ps
        , "?" <> T.intercalate "&" (queryParts qs)
        ]
    PagedQuery ps qs fc -> mconcat
        [ "[GET] "
        , "/" <> T.intercalate "/" ps
        , "?" <> T.intercalate "&" (queryParts qs)
        , " (" <> pack (show fc) <> ")"
        ]
    Command m ps _body ->
        mconcat
            [ "[" <> T.toUpper (pack $ show m) <> "] "
            , "/" <> T.intercalate "/" ps
            ]

queryParts :: QueryString -> [Text]
queryParts = map $ \(k, mv) -> decodeUtf8 k <> "=" <> maybe "" decodeUtf8 mv
