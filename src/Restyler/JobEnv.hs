{-# LANGUAGE NoFieldSelectors #-}

module Restyler.JobEnv
  ( JobEnv (..)
  , jobEnvParser
  ) where

import Restyler.Prelude

import Env qualified
import Restyler.GitHub.Api

data JobEnv = JobEnv
  { githubToken :: GitHubToken
  , repoDisabled :: Bool
  , planRestriction :: Maybe Text
  , planUpgradeUrl :: Maybe URL
  , statsdHost :: Maybe String
  , statsdPort :: Maybe Int
  }

jobEnvParser :: Env.Parser Env.Error JobEnv
jobEnvParser =
  JobEnv
    <$> Env.var
      (Env.str <=< Env.nonempty)
      "GITHUB_ACCESS_TOKEN"
      (Env.help "GitHub access token with write access to the repository")
    <*> Env.switch "REPO_DISABLED" mempty
    <*> optional (Env.var (Env.str <=< Env.nonempty) "PLAN_RESTRICTION" mempty)
    <*> optional (URL <$> Env.var (Env.str <=< Env.nonempty) "PLAN_UPGRADE_URL" mempty)
    <*> optional (Env.var Env.str "STATSD_HOST" mempty)
    <*> optional (Env.var Env.auto "STATSD_PORT" mempty)