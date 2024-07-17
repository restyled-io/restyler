module Restyler.Options.RestyleGHA
  ( EnvOptions (..)
  , envParser
  ) where

import Restyler.Prelude

import Env qualified
import Restyler.GHA
import Restyler.GitHub.Api (GitHubToken, HasGitHubToken (..), envGitHubToken)

data EnvOptions = EnvOptions
  { githubToken :: GitHubToken
  , githubOutput :: GitHubOutput
  }

instance HasGitHubToken EnvOptions where
  githubTokenL = lens (.githubToken) $ \x y -> x {githubToken = y}

instance HasGitHubOutput EnvOptions where
  githubOutputL = lens (.githubOutput) $ \x y -> x {githubOutput = y}

envParser :: Env.Parser Env.Error EnvOptions
envParser =
  EnvOptions
    <$> envGitHubToken
    <*> envGitHubOutput
