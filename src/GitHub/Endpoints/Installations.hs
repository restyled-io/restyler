{-# LANGUAGE OverloadedStrings #-}
module GitHub.Endpoints.Installations
    ( createAccessToken
    , module GitHub.Data
    , module GitHub.Data.Apps
    , module GitHub.Data.AccessTokens
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GitHub.Data
import GitHub.Data.AccessTokens
import GitHub.Data.Apps
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Safe
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Web.JWT as JWT

createAccessToken
    :: MonadIO m
    => Id App           -- ^ GitHub App Id
    -> Text             -- ^ RSA key contents in PEM format
    -> Id Installation  -- ^ The Installation you're accessing
    -> m AccessToken
createAccessToken githubAppId pem installationId = liftIO $ do
    jwt <- encodeJWT githubAppId pem

    request' <- parseRequest
        $ "POST https://api.github.com/installations/"
        <> show (untagId installationId)
        <> "/access_tokens"

    let request = request'
            { requestHeaders =
                [ (hAccept, "application/vnd.github.machine-man-preview+json")
                , (hAuthorization, "Bearer " <> encodeUtf8 jwt)
                , (hUserAgent, "restyled-io")
                ]
            }

    -- Consider: should we refactor GitHub.Client.runGitHub to take a Manager
    -- and share it between this and that?
    mgr <- newManager tlsManagerSettings
    body <- responseBody <$> httpLbs request mgr

    return $ either
        (\e -> error $ unlines
            [ "Error decoding JSON GitHub response"
            , "==================================="
            , "Response body: " ++ L8.unpack body
            , "Error message: " ++ e
            , ""
            ]
        ) id $ eitherDecode body

encodeJWT
    :: Id App -- ^ GitHub App ID
    -> Text     -- ^ RSA key content (e.g. read from downloaded @.pem@ file)
    -> IO JWT.JSON
encodeJWT githubAppId pem = do
    now <- getCurrentTime
    key <- fromJustNote "Invalid RSA data" <$> JWT.rsaKeySecret (T.unpack pem)

    return $ JWT.encodeSigned JWT.RS256 key $ JWT.def
        { JWT.iat = Just $ toNumericDate now
        , JWT.exp = Just $ toNumericDate $ addUTCTime expiration now
        , JWT.iss = Just $ toStringOrURI $ T.pack $ show $ untagId githubAppId
        }
  where
    expiration = 5 * 60 -- 5 minutes

    toNumericDate t = fromJustNote ("Invalid value for NumericDate: " <> show t)
        $ JWT.numericDate
        $ utcTimeToPOSIXSeconds t

    toStringOrURI x = fromJustNote ("Invalid value for StringOrURI: " <> show x)
        $ JWT.stringOrURI x
