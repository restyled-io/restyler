module Restyler.Job
  ( run
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.App.Class
  ( MonadDownloadFile
  , MonadExit
  , MonadProcess
  , MonadSystem
  , exitWithInfo
  )
import Restyler.GHA qualified as GHA
import Restyler.GHA.Output
import Restyler.Git
import Restyler.GitHub.Api
import Restyler.GitHub.PullRequest
import Restyler.JobEnv
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.JobUrl
import Restyler.Options.Manifest
import Restyler.Options.PullRequest
import Restyler.Restrictions
import Restyler.RestyleResult
import Restyler.Setup
import Restyler.Statsd (HasStatsClient)

run
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadExit m
     , MonadProcess m
     , MonadGit m
     , MonadDownloadFile m
     , MonadReader env m
     , HasLogger env
     , HasStatsClient env
     , HasJobEnv env
     , HasGitHubToken env
     , HasGitHubOutput env
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasManifestOption env
     , HasRestrictions env
     )
  => JobUrl
  -- ^ Job URL
  -> PullRequestOption
  -> m ()
run (JobUrl jobUrl) pr = do
  -- TODO: in-line
  restylerSetup pr

  result <- GHA.run pr.repo pr.number

  case result of
    Restyled pullRequest _ -> do
      logInfo "Restyling produced differences"

      patch <- gitFormatPatch $ Just $ unpack pullRequest.head.sha
      withThreadContext ["patch" .= True]
        $ traverse_ (logInfo . (:# []))
        $ T.lines patch

      logInfo ""
      logInfo "NOTE: you can manually apply these fixes by running:"
      logInfo ""
      logInfo "    git checkout <your branch>"
      logInfo $ "    curl " <> getUrl jobUrl <> "/patch | git am" :# []
      logInfo "    git push"
      logInfo ""

      -- -- NB there is the edge-case of switching this off mid-PR. A previously
      -- -- opened Restyle PR would stop updating at that point.
      -- whenConfig (not . cPullRequests) $ do
      --   sendPullRequestStatus $ DifferencesStatus mJobUrl
      --   logInfo
      --     $ "Not creating Restyle PR"
      --     :# ["reason" .= ("disabled by config" :: Text)]
      --   exitWithInfo "Please correct style using the process described above"

      -- let
      --   isDangerous =
      --     pullRequestRepoPublic pullRequest && pullRequestIsFork pullRequest

      --   dangerDetails :: Text
      --   dangerDetails =
      --     "Forks in open source projects could contain unsafe contributions"

      -- when isDangerous $ do
      --   sendPullRequestStatus $ DifferencesStatus mJobUrl
      --   logInfo $ "Not creating Restyle PR" :# ["reason" .= dangerDetails]
      --   exitWithInfo "Please correct style using the process described above"

      -- mRestyledPullRequest <- findRestyledPullRequest pullRequest
      -- url <-
      --   restyledPullRequestHtmlUrl <$> case mRestyledPullRequest of
      --     Nothing -> createRestyledPullRequest pullRequest results
      --     Just pr -> updateRestyledPullRequest pullRequest pr results

      -- sendPullRequestStatus $ DifferencesStatus $ Just url
      exitWithInfo "Restyling successful"
    _ -> do
      -- mRestyledPullRequest <- findRestyledPullRequest pullRequest
      -- traverse_ closeRestyledPullRequest mRestyledPullRequest
      -- sendPullRequestStatus $ NoDifferencesStatus mJobUrl
      exitWithInfo "No style differences found"
