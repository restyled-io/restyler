module Main
  ( main
  ) where

import Restyler.Prelude

import Data.List.NonEmpty (some1)
import Env qualified
import Restyler.App (AppT, runAppT)
import Restyler.Commands.RestyleLocal
import Restyler.Git (ActualGit (..), MonadGit)
import Restyler.HostDirectoryOption
import Restyler.ImageCleanupOption
import Restyler.LogSettingsOption
import Restyler.ManifestOption
import Restyler.Opt qualified as Opt
import Restyler.Options.RestyleLocal
import Restyler.Restrictions

data App = App
  { logger :: Logger
  , options :: Options
  }
  deriving (HasHostDirectoryOption) via (ThroughOptions App)
  deriving (HasImageCleanupOption) via (ThroughOptions App)
  deriving (HasManifestOption) via (ThroughOptions App)
  deriving (HasRestrictions) via (ThroughOptions App)

instance HasOptions App where
  getOptions = (.options)

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

deriving via
  (ActualGit (AppT App m))
  instance
    MonadUnliftIO m => MonadGit (AppT App m)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  env <- Env.parse id envParser
  (opt, paths) <-
    Opt.parse "Restyle local files"
      $ (,)
      <$> optParser
      <*> some1 (Opt.argument Opt.str $ Opt.metavar "PATH")

  let options = env <> opt

  withLogger (resolveLogSettings options.logSettings) $ \logger -> do
    let app = App {logger = logger, options = options}
    void $ runAppT app $ run NullPullRequest $ toList paths
