module Restyler.Options.PullRequest
  ( PullRequestOption (..)
  , readPullRequest
  , optPullRequest
  , optPullRequestArg
  ) where

import Restyler.Prelude

import Options.Applicative
import Restyler.Options.Repository
import Restyler.ReadP

data PullRequestOption = PullRequestOption
  { repo :: RepositoryOption
  , number :: Int
  }
  deriving stock (Eq, Show)

readPullRequest :: String -> Either String PullRequestOption
readPullRequest =
  parseReadP
    $ PullRequestOption
    <$> ( RepositoryOption
            <$> textTill1 '/'
            <*> textTill1 '#'
        )
    <*> digits

optPullRequest :: Parser PullRequestOption
optPullRequest =
  option (eitherReader readPullRequest)
    $ long "pr"
    <> metavar "OWNER/REPO#NUMBER"
    <> help "Pull Request to restyle"

optPullRequestArg :: Parser PullRequestOption
optPullRequestArg =
  argument (eitherReader readPullRequest)
    $ metavar "OWNER/REPO#NUMBER"
    <> help "Pull Request to restyle"
