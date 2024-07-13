module Restyler.GitHubTokenOption
  ( GitHubTokenOption (..)
  , envGitHubTokenOption
  , optGitHubTokenOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative
import Restyler.GitHub.Api

newtype GitHubTokenOption = GitHubTokenOption
  { unGitHubTokenOption :: Last GitHubToken
  }
  deriving newtype (Semigroup)

envGitHubTokenOption :: Env.Parser Env.Error GitHubTokenOption
envGitHubTokenOption =
  GitHubTokenOption
    . Last
    <$> optional (Env.var Env.str "GITHUB_TOKEN" $ Env.help optionHelp)

optGitHubTokenOption :: Parser GitHubTokenOption
optGitHubTokenOption =
  GitHubTokenOption
    . Last
    <$> optional
      ( strOption
          $ long "github-token"
          <> metavar "TOKEN"
          <> help optionHelp
      )

optionHelp :: String
optionHelp = "GitHub token with access to the repository and PR"
