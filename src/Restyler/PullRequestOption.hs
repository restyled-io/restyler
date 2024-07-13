module Restyler.PullRequestOption
  ( PullRequestOption (..)
  , envPullRequestOption
  , optPullRequestOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative

newtype PullRequestOption = PullRequestOption
  { unPullRequestOption :: Last Int
  }
  deriving newtype (Semigroup)

envPullRequestOption :: Env.Parser Env.Error PullRequestOption
envPullRequestOption =
  PullRequestOption
    . Last
    <$> optional (Env.var Env.auto "PULL_REQUEST" $ Env.help optionHelp)

optPullRequestOption :: Parser PullRequestOption
optPullRequestOption =
  PullRequestOption
    . Last
    <$> optional
      ( option
          auto
          $ long "pull-request"
          <> metavar "NUMBER"
          <> help optionHelp
      )

optionHelp :: String
optionHelp = "The PR being restyled"
