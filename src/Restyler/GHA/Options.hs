{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GHA.Options
  ( Options (..)
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
import Restyler.PullRequestNumberOption
import Restyler.RepositoryOption
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettingsOption
  , githubToken :: GitHubTokenOption
  , restrictions :: Restrictions
  , repository :: RepositoryOption
  , pullRequest :: PullRequestNumberOption
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Options)

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
    <*> envGitHubTokenOption
    <*> envRestrictions
    <*> envRepositoryOption
    <*> envPullRequestNumberOption

optOptions :: MonadIO m => m Options
optOptions = liftIO $ execParser $ info (optParser <**> helper) fullDesc

optParser :: Parser Options
optParser =
  Options
    <$> optLogSettingsOption
    <*> optGitHubTokenOption
    <*> optRestrictions
    <*> optRepositoryOption
    <*> optPullRequestNumberOption
