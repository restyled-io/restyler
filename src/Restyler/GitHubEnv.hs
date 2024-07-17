{-# LANGUAGE NoFieldSelectors #-}

-- | Environment variables available on GitHub Actions
module Restyler.GitHubEnv
  ( GitHubEnv (..)
  , githubEnvParser
  ) where

import Restyler.Prelude

import Env qualified
import Restyler.GHA.Output
import Restyler.GitHub.Api (GitHubToken, HasGitHubToken (..), envGitHubToken)

data GitHubEnv = GitHubEnv
  { token :: GitHubToken
  , output :: GitHubOutput
  }

instance HasGitHubToken GitHubEnv where
  githubTokenL = lens (.token) $ \x y -> x {token = y}

instance HasGitHubOutput GitHubEnv where
  githubOutputL = lens (.output) $ \x y -> x {output = y}

githubEnvParser :: Env.Parser Env.Error GitHubEnv
githubEnvParser =
  GitHubEnv
    <$> envGitHubToken
    <*> envGitHubOutput
