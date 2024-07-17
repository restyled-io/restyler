{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.Job.App
  ( App (..)
  , withApp
  ) where

import Restyler.Prelude

import Env qualified
import Restyler.App.Class (HasWorkingDirectory (..))
import Restyler.GHA.Output
import Restyler.GitHub.Api
import Restyler.GitHub.Repository
import Restyler.JobEnv
import Restyler.Local.Options
import Restyler.Opt qualified as Opt
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.JobUrl
import Restyler.Options.LogSettings
import Restyler.Options.Manifest
import Restyler.Options.PullRequest
import Restyler.Restrictions
import Restyler.Statsd

data App = App
  { logger :: Logger
  , options :: Options
  , jobEnv :: JobEnv
  , jobUrl :: JobUrl
  , pullRequest :: PullRequestOption
  , workingDirectory :: FilePath
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

instance HasJobEnv App where
  getJobEnv = (.jobEnv)

instance HasGitHubToken App where
  getGitHubToken = getGitHubToken . (.jobEnv)

instance HasWorkingDirectory App where
  workingDirectoryL = lens (.workingDirectory) $ \x y -> x {workingDirectory = y}

instance HasStatsClient App where
  statsClientL = lens (.statsClient) $ \x y -> x {statsClient = y}

withApp :: (App -> IO a) -> IO a
withApp f = do
  (jobEnv, env) <-
    Env.parse id
      $ (,)
      <$> jobEnvParser
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
    withStatsClient jobEnv.statsdHost jobEnv.statsdPort statsdTags $ \statsClient -> do
      withSystemTempDirectory "restyler-" $ \workingDirectory -> do
        f
          $ App
            { logger
            , options
            , jobEnv
            , jobUrl
            , pullRequest
            , workingDirectory
            , statsClient
            }
