module Restyler.Options.NoCommit
  ( NoCommitOption (..)
  , HasNoCommitOption (..)
  , getNoCommit
  , envNoCommitOption
  , optNoCommitOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative

newtype NoCommitOption = NoCommitOption
  { unwrap :: Any
  }
  deriving newtype (Semigroup, Monoid)

class HasNoCommitOption a where
  getNoCommitOption :: a -> NoCommitOption

getNoCommit :: (MonadReader env m, HasNoCommitOption env) => m Bool
getNoCommit = asks $ getAny . (.unwrap) . getNoCommitOption

envNoCommitOption :: Env.Parser Env.Error NoCommitOption
envNoCommitOption =
  NoCommitOption . Any <$> Env.switch "NO_COMMIT" (Env.help optionHelp)

optNoCommitOption :: Parser NoCommitOption
optNoCommitOption =
  NoCommitOption . Any <$> switch (long "no-commit" <> help optionHelp)

optionHelp :: String
optionHelp = "Don't make commits for restyle changes"
