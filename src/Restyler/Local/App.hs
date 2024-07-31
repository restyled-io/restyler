module Restyler.Local.App
  ( App (..)
  , withApp
  ) where

import Restyler.Prelude

import Data.List.NonEmpty (some1)
import Env qualified
import Restyler.Local.Options
import Restyler.Opt qualified as Opt
import Restyler.Options.DryRun
import Restyler.Options.FailOnDifferences
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.LogSettings
import Restyler.Options.Manifest
import Restyler.Options.NoCommit
import Restyler.Options.NoPull
import Restyler.Restrictions

data App = App
  { logger :: Logger
  , options :: Options
  , pullRequestJson :: Maybe FilePath
  , paths :: NonEmpty FilePath
  }
  deriving (HasDryRunOption) via (ThroughOptions App)
  deriving (HasFailOnDifferencesOption) via (ThroughOptions App)
  deriving (HasHostDirectoryOption) via (ThroughOptions App)
  deriving (HasImageCleanupOption) via (ThroughOptions App)
  deriving (HasManifestOption) via (ThroughOptions App)
  deriving (HasNoCommitOption) via (ThroughOptions App)
  deriving (HasNoPullOption) via (ThroughOptions App)
  deriving (HasRestrictions) via (ThroughOptions App)

instance HasOptions App where
  getOptions = (.options)

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

withApp :: (App -> IO a) -> IO a
withApp f = do
  (opt, pullRequestJson, paths) <-
    Opt.parse "Restyle local files" (Env.helpDoc envParser) optp

  env <- Env.parse id envParser

  let options = env <> opt

  withLogger (resolveLogSettings options.logSettings) $ \logger -> do
    f $ App {logger, options, pullRequestJson, paths}
 where
  optp :: Opt.Parser (Options, Maybe FilePath, NonEmpty FilePath)
  optp =
    (,,)
      <$> optParser
      <*> optional (Opt.option Opt.str $ Opt.long "pull-request-json" <> Opt.hidden)
      <*> some1 (Opt.argument Opt.str $ Opt.metavar "PATH")
