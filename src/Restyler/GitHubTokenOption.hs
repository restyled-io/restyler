{-# LANGUAGE DerivingVia #-}

module Restyler.GitHubTokenOption
  ( GitHubTokenOption (..)
  , envGitHubTokenOption
  , optGitHubTokenOption
  ) where

import Restyler.Prelude hiding (Last (..))

import Data.Semigroup (Last (..))
import Env qualified
import Options.Applicative
import Restyler.GitHub.Api

newtype GitHubTokenOption = GitHubTokenOption
  { unGitHubTokenOption :: GitHubToken
  }
  deriving (Semigroup) via (Last GitHubTokenOption)

envGitHubTokenOption :: Env.Parser Env.Error GitHubTokenOption
envGitHubTokenOption =
  GitHubTokenOption
    <$> Env.var Env.str "GITHUB_TOKEN" (Env.help optionHelp)

optGitHubTokenOption :: Parser GitHubTokenOption
optGitHubTokenOption =
  GitHubTokenOption
    <$> strOption
      ( long "github-token"
          <> metavar "TOKEN"
          <> help optionHelp
      )

optionHelp :: String
optionHelp = "GitHub token with access to the repository and PR"
