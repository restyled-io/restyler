module Restyler.GitHub.Api
  ( HasGitHubToken
  , GitHubToken (..)
  , getOne
  , getAll
  ) where

import Restyler.Prelude

import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization)

class HasGitHubToken env where
  githubTokenL :: Lens' env GitHubToken

newtype GitHubToken = GitHubToken
  { unGitHubToken :: Text
  }

getOne
  :: ( MonadIO m
     , MonadReader env m
     , HasGitHubToken env
     , FromJSON a
     )
  => String
  -> m a
getOne url = do
  token <- view githubTokenL
  req <- liftIO $ parseRequest url
  resp <- httpJSON $ addRequestHeader hAuthorization (toBearer token) req
  pure $ getResponseBody resp

getAll
  :: ( MonadIO m
     , MonadReader env m
     , HasGitHubToken env
     , FromJSON a
     )
  => String
  -> m [a]
getAll = getOne -- TODO: pagination

toBearer :: GitHubToken -> ByteString
toBearer = ("Bearer " <>) . encodeUtf8 . unGitHubToken
