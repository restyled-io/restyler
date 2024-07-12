module Restyler.PullRequestNumberOption
  ( PullRequestNumberOption (..)
  , unPullRequestNumberOption
  , envPullRequestNumberOption
  , optPullRequestNumberOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative

newtype PullRequestNumberOption = PullRequestNumberOption (Last Int)
  deriving newtype (Semigroup, Monoid)

toPullRequestNumberOption :: Maybe Int -> PullRequestNumberOption
toPullRequestNumberOption = PullRequestNumberOption . Last

unPullRequestNumberOption :: PullRequestNumberOption -> Maybe Int
unPullRequestNumberOption (PullRequestNumberOption x) = getLast x

envPullRequestNumberOption :: Env.Parser Env.Error PullRequestNumberOption
envPullRequestNumberOption =
  toPullRequestNumberOption
    <$> optional (Env.var Env.auto "PULL_REQUEST" $ Env.help optionHelp)

optPullRequestNumberOption :: Parser PullRequestNumberOption
optPullRequestNumberOption =
  toPullRequestNumberOption
    <$> optional
      ( option auto
          $ long "pull-request"
          <> metavar "NUMBER"
          <> help optionHelp
      )

optionHelp :: String
optionHelp = "The PR being restyled"
