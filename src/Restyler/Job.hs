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
import Restyler.Clone
import Restyler.Config
import Restyler.GHA qualified as GHA
import Restyler.GHA.Output
import Restyler.Git
import Restyler.GitHub.Api
import Restyler.GitHub.Commit.Status
import Restyler.GitHub.PullRequest
import Restyler.Job.PlanUpgradeRequired
import Restyler.JobEnv
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options.JobUrl
import Restyler.Options.Manifest
import Restyler.Options.PullRequest
import Restyler.Restrictions
import Restyler.RestyleResult
import Restyler.RestyledPullRequest
  ( closeRestyledPullRequest
  , createRestyledPullRequest
  , findRestyledPullRequest
  , updateRestyledPullRequest
  )
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
  env <- asks getJobEnv

  when env.repoDisabled
    $ exitWithInfo
    $ fromString
    $ "This repository has been disabled for possible abuse."
    <> " If you believe this is an error, please reach out to"
    <> " support@restyled.io"

  for_ env.planRestriction $ \planRestriction -> do
    throwIO $ PlanUpgradeRequired planRestriction env.planUpgradeUrl

  clonePullRequest pr

  result <- GHA.run pr.repo pr.number

  case result of
    Restyled pullRequest results -> do
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

      let
        -- TODO
        config :: Config
        config = error "TODO"

        -- TODO
        -- let isDangerous = pullRequestRepoPublic pullRequest && pullRequestIsFork pullRequest
        -- "Forks in open source projects could contain unsafe contributions"
        mDetails :: Maybe Text
        mDetails = Nothing

      url <- case (cPullRequests config, mDetails) of
        (False, _) -> do
          logInfo
            $ "Not creating Restyle PR"
            :# ["reason" .= ("disabled by config" :: Text)]
          logInfo "Please correct style using the process described above"
          pure jobUrl
        (True, Just details) -> do
          logInfo $ "Not creating Restyle PR" :# ["reason" .= details]
          logInfo "Please correct style using the process described above"
          pure jobUrl
        (True, Nothing) -> do
          mRestyledPullRequest <- findRestyledPullRequest pullRequest
          getHtmlUrl <$> case mRestyledPullRequest of
            Nothing -> createRestyledPullRequest pullRequest results
            Just restyledPullRequest -> updateRestyledPullRequest pullRequest restyledPullRequest results

      setPullRequestStatus
        pullRequest
        CommitStatusFailure
        url
        "Restyling found differences"
      exitWithInfo "Restyling successful"
    _ -> do
      let
        -- TODO
        pullRequest :: PullRequest
        pullRequest = error "TODO"
      mRestyledPullRequest <- findRestyledPullRequest pullRequest
      traverse_ closeRestyledPullRequest mRestyledPullRequest
      setPullRequestStatus pullRequest CommitStatusSuccess jobUrl "No differences"
      exitWithInfo "No style differences found"
