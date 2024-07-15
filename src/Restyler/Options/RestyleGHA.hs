{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Restyler.Options.RestyleGHA
  ( EnvOptions (..)
  , envOptions
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

envOptions :: IO EnvOptions
envOptions =
  Env.parse id
    $ EnvOptions
    <$> envGitHubToken
    <*> envGitHubOutput
