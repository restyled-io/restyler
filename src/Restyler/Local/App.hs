module Restyler.Local.App
  ( App (..)
  , withApp
  ) where

import Restyler.Prelude

import Data.List.NonEmpty (some1)
import Env qualified
import Restyler.Local.Options
import Restyler.Opt qualified as Opt
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.LogSettings
import Restyler.Options.Manifest
import Restyler.Options.NoCommit
import Restyler.Restrictions

data App = App
  { logger :: Logger
  , options :: Options
  , paths :: NonEmpty FilePath
  }
  deriving (HasHostDirectoryOption) via (ThroughOptions App)
  deriving (HasImageCleanupOption) via (ThroughOptions App)
  deriving (HasManifestOption) via (ThroughOptions App)
  deriving (HasNoCommitOption) via (ThroughOptions App)
  deriving (HasRestrictions) via (ThroughOptions App)

instance HasOptions App where
  getOptions = (.options)

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

withApp :: (App -> IO a) -> IO a
withApp f = do
  env <- Env.parse id envParser
  (opt, paths) <-
    Opt.parse "Restyle local files"
      $ (,)
      <$> optParser
      <*> some1 (Opt.argument Opt.str $ Opt.metavar "PATH")

  let options = env <> opt

  withLogger (resolveLogSettings options.logSettings) $ \logger -> do
    f $ App {logger, options, paths}
