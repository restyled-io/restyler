-- | Environment variables available on GitHub Actions
module Restyler.GHA.GitHubEnv
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
  getGitHubToken = (.token)

instance HasGitHubOutput GitHubEnv where
  getGitHubOutput = (.output)

githubEnvParser :: Env.Parser Env.Error GitHubEnv
githubEnvParser =
  GitHubEnv
    <$> envGitHubToken
    <*> envGitHubOutput
