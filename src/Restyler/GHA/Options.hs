{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GHA.Options
  ( Options (..)
  , GitHubOptions (..)
  , getOptions
  , envOptions
  , optOptions
  ) where

import Restyler.Prelude

import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Env qualified
import Options.Applicative
import Restyler.GitHubTokenOption
import Restyler.LogSettingsOption
import Restyler.PullRequestOption
import Restyler.RepositoryOption
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettingsOption
  , restrictions :: Restrictions
  , github :: Maybe GitHubOptions
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Options)

data GitHubOptions = GitHubOptions
  { githubToken :: GitHubTokenOption
  , repository :: RepositoryOption
  , pullRequest :: PullRequestOption
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid GitHubOptions)

getOptions :: MonadIO m => m Options
getOptions =
  (<>)
    <$> envOptions
    <*> optOptions

envOptions :: MonadIO m => m Options
envOptions = liftIO $ Env.parse id envParser

envParser :: Env.Parser Env.Error Options
envParser =
  Options
    <$> envLogSettingsOption
    <*> envRestrictions
    <*> optional envGitHubOptions

envGitHubOptions :: Env.Parser Env.Error GitHubOptions
envGitHubOptions =
  GitHubOptions
    <$> envGitHubTokenOption
    <*> envRepositoryOption
    <*> envPullRequestOption

optOptions :: MonadIO m => m Options
optOptions = liftIO $ execParser $ info (optParser <**> helper) fullDesc

optParser :: Parser Options
optParser =
  Options
    <$> optLogSettingsOption
    <*> optRestrictions
    <*> optional optGitHubOptions

optGitHubOptions :: Parser GitHubOptions
optGitHubOptions =
  GitHubOptions
    <$> optGitHubTokenOption
    <*> optRepositoryOption
    <*> optPullRequestOption
