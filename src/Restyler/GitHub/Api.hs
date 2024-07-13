module Restyler.GitHub.Api
  ( GitHubToken (..)
  , getOne
  , getAll
  ) where

import Restyler.Prelude

import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization)

newtype GitHubToken = GitHubToken
  { unGitHubToken :: Text
  }
  deriving newtype (IsString)

getOne :: (MonadIO m, FromJSON a) => GitHubToken -> String -> m a
getOne token url = do
  req <- liftIO $ parseRequest url
  resp <- httpJSON $ addRequestHeader hAuthorization (toBearer token) req
  pure $ getResponseBody resp

getAll :: (MonadIO m, FromJSON a) => GitHubToken -> String -> m [a]
getAll = getOne -- TODO: pagination

toBearer :: GitHubToken -> ByteString
toBearer = ("Bearer " <>) . encodeUtf8 . unGitHubToken
