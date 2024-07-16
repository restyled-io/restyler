module Main
  ( main
  ) where

import Restyler.Prelude

import Env qualified
import Restyler.App (AppT)
import Restyler.CLI qualified as CLI
import Restyler.Commands.RestyleGHA
import Restyler.GHA
import Restyler.Git (ActualGit (..), MonadGit)
import Restyler.GitHub.Api
import Restyler.HostDirectoryOption
import Restyler.ImageCleanupOption
import Restyler.LogSettingsOption
import Restyler.ManifestOption
import Restyler.Opt (PullRequestOption)
import Restyler.Opt qualified as Opt
import Restyler.Options.RestyleGHA
import Restyler.Options.RestyleLocal
  ( HasOptions (..)
  , Options (..)
  , ThroughOptions (..)
  )
import Restyler.Options.RestyleLocal qualified as RestyleLocal
import Restyler.Restrictions

data App = App
  { logger :: Logger
  , env :: EnvOptions
  , options :: Options
  , pullRequest :: PullRequestOption
  }
  deriving (HasHostDirectoryOption) via (ThroughOptions App)
  deriving (HasImageCleanupOption) via (ThroughOptions App)
  deriving (HasManifestOption) via (ThroughOptions App)
  deriving (HasRestrictions) via (ThroughOptions App)

instance HasOptions App where
  getOptions = (.options)

envL :: Lens' App EnvOptions
envL = lens (.env) $ \x y -> x {env = y}

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasGitHubToken App where
  githubTokenL = envL . githubTokenL

instance HasGitHubOutput App where
  githubOutputL = envL . githubOutputL

deriving via
  (ActualGit (AppT App m))
  instance
    MonadUnliftIO m => MonadGit (AppT App m)

withApp :: (App -> IO a) -> IO a
withApp f = do
  (env, envLocal) <-
    Env.parse id
      $ (,)
      <$> envParser
      <*> RestyleLocal.envParser

  (optLocal, pr) <-
    Opt.parse "Restyle on GitHub Actions"
      $ (,)
      <$> RestyleLocal.optParser
      <*> Opt.optPullRequest

  let options = envLocal <> optLocal

  withLogger (resolveLogSettings options.logSettings) $ \logger -> do
    f $ App {logger = logger, env = env, options = options, pullRequest = pr}

main :: IO ()
main = CLI.main withApp $ do
  pr <- asks (.pullRequest)
  void $ run pr.repo pr.number
