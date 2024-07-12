module Restyler.RepositoryOption
  ( RepositoryOption (..)
  , envRepositoryOption
  , optRepositoryOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative
import Restyler.GitHub.Repository

newtype RepositoryOption = RepositoryOption
  { unRepositoryOption :: Last Repository
  }
  deriving newtype (Semigroup)

envRepositoryOption :: Env.Parser Env.Error RepositoryOption
envRepositoryOption =
  RepositoryOption
    . Last
    <$> optional
      ( Env.var (first Env.UnreadError . parseRepository) "REPOSITORY"
          $ Env.help optionHelp
      )

optRepositoryOption :: Parser RepositoryOption
optRepositoryOption =
  RepositoryOption
    . Last
    <$> optional
      ( option (eitherReader parseRepository)
          $ long "repo"
          <> metavar "OWNER/REPO"
          <> help optionHelp
      )

optionHelp :: String
optionHelp = "Repository being restyled"
