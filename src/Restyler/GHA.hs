module Restyler.GHA
  ( run
  ) where

import Restyler.Prelude

import Restyler.AnnotatedException
import Restyler.App.Class (MonadDownloadFile, MonadProcess, MonadSystem)
import Restyler.GHA.Output
import Restyler.Git (MonadGit)
import Restyler.GitHub.Api
import Restyler.GitHub.PullRequest
import Restyler.GitHub.PullRequest.File
import Restyler.Local qualified as Local
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.Manifest
import Restyler.Options.Repository
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
     , HasCallStack
     )
  => RepositoryOption
  -> Int
  -> m (RestyleResult PullRequest)
run repo pr = do
  pullRequest <- getPullRequest repo pr
  checkpoint (Annotation pullRequest) $ do
    logInfo $ "Handling PR" :# objectToPairs pullRequest
    paths <- mapMaybe pullRequestFileToChangedPath <$> getPullRequestFiles repo pr
    Local.run pullRequest paths `with` setRestylerResultOutputs
