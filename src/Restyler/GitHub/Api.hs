{-# LANGUAGE UndecidableInstances #-}

module Restyler.GitHub.Api
  ( MonadGitHub (..)

    -- * @DerivingVia@
  , GitHubToken (..)
  , envGitHubToken
  , HasGitHubToken (..)
  , ActualGitHub (..)
  ) where

import Restyler.Prelude

import Data.Aeson
import Env qualified
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization)
import Restyler.GitHub.PullRequest
import Restyler.GitHub.PullRequest.File
import Restyler.GitHub.Repository

class Monad m => MonadGitHub m where
  getPullRequest :: Repository -> Int -> m PullRequest
  getPullRequest repo pr =
    getOne
      $ "https://api.github.com/repos/"
      <> unpack repo.owner
      <> "/"
      <> unpack repo.repo
      <> "/pulls/"
      <> show pr

  getPullRequestFiles :: Repository -> Int -> m [PullRequestFile]
  getPullRequestFiles repo pr =
    getAll
      $ "https://api.github.com/repos/"
      <> unpack repo.owner
      <> "/"
      <> unpack repo.repo
      <> "/pulls/"
      <> show pr
      <> "/files"

  getAll :: FromJSON a => String -> m [a]
  getAll = getOne -- TODO: pagination

  getOne :: FromJSON a => String -> m a

newtype GitHubToken = GitHubToken
  { unGitHubToken :: Text
  }
  deriving newtype (IsString)

envGitHubToken :: Env.Parser Env.Error GitHubToken
envGitHubToken =
  Env.var Env.str "GITHUB_TOKEN"
    $ Env.help "GitHub token with access to the repo and PR"

githubTokenToBearer :: GitHubToken -> ByteString
githubTokenToBearer = ("Bearer " <>) . encodeUtf8 . unGitHubToken

class HasGitHubToken env where
  githubTokenL :: Lens' env GitHubToken

instance HasGitHubToken GitHubToken where
  githubTokenL = id

newtype ActualGitHub m a = ActualGitHub
  { unwrap :: m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadReader env
    , MonadIO
    )

instance (Monad m, MonadIO m, MonadReader env m, HasGitHubToken env) => MonadGitHub m where
  getOne url = do
    auth <- view $ githubTokenL . to githubTokenToBearer
    req <- liftIO $ parseRequest url
    resp <- httpJSON $ addRequestHeader hAuthorization auth req
    pure $ getResponseBody resp
