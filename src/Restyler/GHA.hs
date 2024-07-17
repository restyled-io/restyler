module Restyler.GHA
  ( run
  ) where

import Restyler.Prelude

import Restyler.App.Class (MonadDownloadFile, MonadProcess, MonadSystem)
import Restyler.GHA.Output
import Restyler.Git (MonadGit)
import Restyler.GitHub.Api
import Restyler.GitHub.PullRequest.File
import Restyler.GitHub.Repository
import Restyler.Local qualified as Local
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.Manifest
import Restyler.Restrictions
import Restyler.RestyleResult

run
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadDownloadFile m
     , MonadSystem m
     , MonadProcess m
     , MonadGit m
     , MonadReader env m
     , HasLogger env
     , HasGitHubToken env
     , HasGitHubOutput env
     , HasRestrictions env
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasManifestOption env
     )
  => Repository
  -> Int
  -> m RestyleResult
run repo pr = do
  pullRequest <- getPullRequest repo pr
  logInfo $ "Handling PR" :# objectToPairs pullRequest

  paths <- mapMaybe pullRequestFileToChangedPath <$> getPullRequestFiles repo pr
  traverse_ (logDebug . ("Path" :#) . objectToPairs) paths

  result <- Local.run pullRequest paths
  result <$ setRestylerResultOutputs pullRequest result
