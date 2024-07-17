-- | Environment variables available on GitHub Actions
module Restyler.GitHubEnv
  ( GitHubEnv (..)
  , githubEnvParser
  ) where

import Restyler.Prelude

import Env qualified
import Restyler.GHA
import Restyler.GitHub.Api (GitHubToken, HasGitHubToken (..), envGitHubToken)

data GitHubEnv = GitHubEnv
  { token :: GitHubToken
  , output :: GitHubOutput
  }

instance HasGitHubToken GitHubEnv where
  githubTokenL = lens (.githubToken) $ \x y -> x {githubToken = y}

instance HasGitHubOutput GitHubEnv where
  githubOutputL = lens (.githubOutput) $ \x y -> x {githubOutput = y}

githubEnvParser :: Env.Parser Env.Error GitHubEnv
githubEnvParser =
  GitHubEnv
    <$> envGitHubToken
    <*> envGitHubOutput
