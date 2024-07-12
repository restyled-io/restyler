module Restyler.GitHubTokenOption
  ( GitHubTokenOption (..)
  , toGitHubTokenOption
  , unGitHubTokenOption
  , getGitHubTokenOption
  , envGitHubTokenOption
  , optGitHubTokenOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative
import Restyler.GitHub.Api

newtype GitHubTokenOption = GitHubTokenOption (Last GitHubToken)
  deriving newtype (Semigroup, Monoid)

toGitHubTokenOption :: Maybe GitHubToken -> GitHubTokenOption
toGitHubTokenOption = GitHubTokenOption . Last

unGitHubTokenOption :: GitHubTokenOption -> Maybe GitHubToken
unGitHubTokenOption (GitHubTokenOption x) = getLast x

getGitHubTokenOption :: GitHubTokenOption -> GitHubToken
getGitHubTokenOption = fromMaybe "invalid" . unGitHubTokenOption -- TODO

envGitHubTokenOption :: Env.Parser Env.Error GitHubTokenOption
envGitHubTokenOption =
  toGitHubTokenOption
    <$> optional (Env.var Env.str "GITHUB_TOKEN" $ Env.help optionHelp)

optGitHubTokenOption :: Parser GitHubTokenOption
optGitHubTokenOption =
  toGitHubTokenOption
    <$> optional
      ( strOption
          $ long "github-token"
          <> metavar "TOKEN"
          <> help optionHelp
      )

optionHelp :: String
optionHelp = "GitHub token with access to the repository and PR"
