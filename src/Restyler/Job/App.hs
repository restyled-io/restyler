module Restyler.Job.App
  ( App (..)
  , withApp
  ) where

import Restyler.Prelude

import Env qualified
import Restyler.GHA.Output
import Restyler.GitHub.Api
import Restyler.Job.AgentEnv
import Restyler.Local.Options
import Restyler.Opt qualified as Opt
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.JobUrl
import Restyler.Options.LogSettings
import Restyler.Options.Manifest
import Restyler.Options.PullRequest
import Restyler.Options.Repository
import Restyler.Restrictions
import Restyler.Statsd
import System.Directory qualified as Directory

data App = App
  { logger :: Logger
  , options :: Options
  , agentEnv :: AgentEnv
  , jobUrl :: JobUrl
  , pullRequest :: PullRequestOption
  , statsClient :: StatsClient
  }
  deriving (HasManifestOption) via (ThroughOptions App)
  deriving (HasImageCleanupOption) via (ThroughOptions App)
  deriving (HasHostDirectoryOption) via (ThroughOptions App)
  deriving (HasRestrictions) via (ThroughOptions App)
  deriving (HasGitHubOutput) via (NullGitHubOutput App)

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasOptions App where
  getOptions = (.options)

instance HasAgentEnv App where
  getAgentEnv = (.agentEnv)

instance HasGitHubToken App where
  getGitHubToken = getGitHubToken . (.agentEnv)

instance HasStatsClient App where
  statsClientL = lens (.statsClient) $ \x y -> x {statsClient = y}

withApp :: (App -> IO a) -> IO a
withApp f = do
  (agentEnv, env) <-
    Env.parse id
      $ (,)
      <$> agentEnvParser
      <*> envParser

  (opt, jobUrl, pullRequest) <-
    Opt.parse "Restyle on GitHub Actions"
      $ (,,)
      <$> optParser
      <*> optJobUrl
      <*> optPullRequestArg

  let
    options = env <> opt

    statsdTags :: [(Text, Text)]
    statsdTags =
      [ ("repo", pullRequest.repo.owner <> "/" <> pullRequest.repo.repo)
      ]

  withLogger (resolveLogSettings options.logSettings) $ \logger -> do
    withStatsClient agentEnv.statsdHost agentEnv.statsdPort statsdTags $ \statsClient -> do
      withSystemTempDirectory "restyler-" $ \tmp -> do
        Directory.withCurrentDirectory tmp $ do
          f
            $ App
              { logger
              , options
              , agentEnv
              , jobUrl
              , pullRequest
              , statsClient
              }
