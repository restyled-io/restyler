module Restyler.RepositoryOption
  ( RepositoryOption (..)
  , unRepositoryOption
  , envRepositoryOption
  , optRepositoryOption
  , Repository (..)
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative
import Restyler.GitHub.Repository

newtype RepositoryOption = RepositoryOption (Last Repository)
  deriving newtype (Semigroup, Monoid)

toRepositoryOption :: Maybe Repository -> RepositoryOption
toRepositoryOption = RepositoryOption . Last

unRepositoryOption :: RepositoryOption -> Maybe Repository
unRepositoryOption (RepositoryOption x) = getLast x

envRepositoryOption :: Env.Parser Env.Error RepositoryOption
envRepositoryOption =
  toRepositoryOption
    <$> optional
      ( Env.var (first Env.UnreadError . parseRepository) "REPOSITORY"
          $ Env.help optionHelp
      )

optRepositoryOption :: Parser RepositoryOption
optRepositoryOption =
  toRepositoryOption
    <$> optional
      ( option (eitherReader parseRepository)
          $ long "repo"
          <> metavar "OWNER/REPO"
          <> help optionHelp
      )

optionHelp :: String
optionHelp = "Repository being restyled"
