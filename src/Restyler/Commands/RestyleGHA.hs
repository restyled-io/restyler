-- | @restyle --pr "OWNER/REPO#NUMBER"@
module Restyler.Commands.RestyleGHA
  ( run
  ) where

import Restyler.Prelude

import Restyler.App.Class (MonadDownloadFile, MonadProcess, MonadSystem)
import Restyler.Commands.RestyleLocal qualified as RestyleLocal
import Restyler.GHA
import Restyler.Git (MonadGit)
import Restyler.GitHub.Api
import Restyler.GitHub.PullRequest.File
import Restyler.GitHub.Repository
import Restyler.HostDirectoryOption
import Restyler.ImageCleanupOption
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

  result <- RestyleLocal.run pullRequest paths
  result <$ setRestylerResultOutputs pullRequest result
