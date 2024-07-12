{-# LANGUAGE DerivingVia #-}

module Restyler.PullRequestOption
  ( PullRequestOption (..)
  , envPullRequestOption
  , optPullRequestOption
  ) where

import Restyler.Prelude hiding (Last (..))

import Data.Semigroup (Last (..))
import Env qualified
import Options.Applicative

newtype PullRequestOption = PullRequestOption
  { unPullRequestOption :: Int
  }
  deriving (Semigroup) via (Last PullRequestOption)

envPullRequestOption :: Env.Parser Env.Error PullRequestOption
envPullRequestOption =
  PullRequestOption
    <$> Env.var Env.auto "PULL_REQUEST" (Env.help optionHelp)

optPullRequestOption :: Parser PullRequestOption
optPullRequestOption =
  PullRequestOption
    <$> option
      auto
      ( long "pull-request"
          <> metavar "NUMBER"
          <> help optionHelp
      )

optionHelp :: String
optionHelp = "The PR being restyled"
