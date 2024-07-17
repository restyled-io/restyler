module Restyler.Options.PullRequest
  ( PullRequestOption (..)
  , readPullRequest
  , optPullRequest
  ) where

import Restyler.Prelude

import Options.Applicative
import Restyler.GitHub.Repository
import Restyler.ReadP

data PullRequestOption = PullRequestOption
  { repo :: Repository
  , number :: Int
  }
  deriving stock (Eq, Show)

readPullRequest :: String -> Either String PullRequestOption
readPullRequest =
  parseReadP
    $ PullRequestOption
    <$> ( Repository
            <$> textTill1 '/'
            <*> textTill1 '#'
        )
    <*> digits

optPullRequest :: Parser PullRequestOption
optPullRequest =
  option (eitherReader readPullRequest)
    $ long "pr"
    <> metavar "OWNER/REPO#NUMBER"
    <> help "Pull Request to restyler"
