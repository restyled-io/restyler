-- | The required ENV vars when run on a Restyled Agent
module Restyler.Job.AgentEnv
  ( HasAgentEnv (..)
  , AgentEnv (..)
  , agentEnvParser
  , assertAgentEnv
  ) where

import Restyler.Prelude

import Env qualified
import Restyler.AnnotatedException (throw)
import Restyler.GitHub.Api
import Restyler.Job.PlanUpgradeRequired
import Restyler.Job.RepoDisabled

class HasAgentEnv a where
  getAgentEnv :: a -> AgentEnv

instance HasAgentEnv AgentEnv where
  getAgentEnv = id

data AgentEnv = AgentEnv
  { githubToken :: GitHubToken
  , repoDisabled :: Bool
  , planRestriction :: Maybe Text
  , planUpgradeUrl :: Maybe URL
  , statsdHost :: Maybe String
  , statsdPort :: Maybe Int
  }

instance HasGitHubToken AgentEnv where
  getGitHubToken = (.githubToken)

agentEnvParser :: Env.Parser Env.Error AgentEnv
agentEnvParser =
  AgentEnv
    <$> Env.var
      (Env.str <=< Env.nonempty)
      "GITHUB_ACCESS_TOKEN"
      (Env.help "GitHub access token with write access to the repository")
    <*> Env.switch "REPO_DISABLED" mempty
    <*> optional (Env.var (Env.str <=< Env.nonempty) "PLAN_RESTRICTION" mempty)
    <*> optional (URL <$> Env.var (Env.str <=< Env.nonempty) "PLAN_UPGRADE_URL" mempty)
    <*> optional (Env.var Env.str "STATSD_HOST" mempty)
    <*> optional (Env.var Env.auto "STATSD_PORT" mempty)

assertAgentEnv :: (MonadIO m, MonadReader env m, HasAgentEnv env) => m ()
assertAgentEnv = do
  env <- asks getAgentEnv
  when env.repoDisabled $ throw RepoDisabled
  for_ env.planRestriction $ throw . flip PlanUpgradeRequired env.planUpgradeUrl
