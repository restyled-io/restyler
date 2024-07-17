{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GHA.App
  ( App (..)
  , withApp
  ) where

import Restyler.Prelude

import Env qualified
import Restyler.GHA.Output
import Restyler.GitHub.Api
import Restyler.GitHubEnv
import Restyler.Local.Options
import Restyler.Opt qualified as Opt
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.LogSettings
import Restyler.Options.Manifest
import Restyler.Options.PullRequest
import Restyler.Restrictions

data App = App
  { logger :: Logger
  , options :: Options
  , githubEnv :: GitHubEnv
  , pullRequest :: PullRequestOption
  }
  deriving (HasHostDirectoryOption) via (ThroughOptions App)
  deriving (HasImageCleanupOption) via (ThroughOptions App)
  deriving (HasManifestOption) via (ThroughOptions App)
  deriving (HasRestrictions) via (ThroughOptions App)

instance HasOptions App where
  getOptions = (.options)

githubEnvL :: Lens' App GitHubEnv
githubEnvL = lens (.githubEnv) $ \x y -> x {githubEnv = y}

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasGitHubToken App where
  githubTokenL = githubEnvL . githubTokenL

instance HasGitHubOutput App where
  githubOutputL = githubEnvL . githubOutputL

withApp :: (App -> IO a) -> IO a
withApp f = do
  (githubEnv, env) <-
    Env.parse id
      $ (,)
      <$> githubEnvParser
      <*> envParser

  (opt, pullRequest) <-
    Opt.parse "Restyle on GitHub Actions"
      $ (,)
      <$> optParser
      <*> optPullRequest

  let options = env <> opt

  withLogger (resolveLogSettings options.logSettings) $ \logger -> do
    f $ App {logger, options, githubEnv, pullRequest}
