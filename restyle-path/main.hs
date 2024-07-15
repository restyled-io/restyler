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

optionsL :: Lens' App Options
optionsL = lens (.options) $ \x y -> x {options = y}

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasRestrictions App where
  restrictionsL = optionsL . restrictionsL

instance HasHostDirectoryOption App where
  hostDirectoryOptionL = optionsL . hostDirectoryOptionL

instance HasManifestOption App where
  manifestOptionL = noManifestOptionL

instance HasImageCleanupOption App where
  imageCleanupOptionL = noImageCleanupOptionL

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
