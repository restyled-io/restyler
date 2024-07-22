module Restyler.GHA
  ( run
  ) where

import Restyler.Prelude

import Restyler.App.Class (MonadDownloadFile, MonadSystem)
import Restyler.Docker (MonadDocker)
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
     , MonadGitHub m
     , MonadGit m
     , MonadDocker m
     , MonadReader env m
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
  logInfo
    $ "Handling PR"
    :# [ "owner" .= pullRequest.base.repo.owner.login
       , "repo" .= pullRequest.base.repo.name
       , "number" .= pullRequest.number
       , "state" .= pullRequest.state
       , "title" .= pullRequest.title
       , "base" .= pullRequest.base.ref
       , "head" .= pullRequest.head.ref
       ]
  paths <- mapMaybe pullRequestFileToChangedPath <$> getPullRequestFiles repo pr
  Local.run pullRequest paths `with` setRestylerResultOutputs
