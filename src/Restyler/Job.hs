module Restyler.Job
  ( run
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Restyler.AnnotatedException
import Restyler.App.Class (MonadDownloadFile, MonadProcess, MonadSystem)
import Restyler.Clone
import Restyler.Config
import Restyler.DangerousPullRequest
import Restyler.ErrorMetadata
import Restyler.GHA qualified as GHA
import Restyler.GHA.Output
import Restyler.Git
import Restyler.GitHub.Api
import Restyler.GitHub.Commit.Status
import Restyler.GitHub.PullRequest
import Restyler.Job.PlanUpgradeRequired
import Restyler.Job.RepoDisabled
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
import Restyler.Statsd qualified as Statsd

run
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
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
  -> m (RestyleResult PullRequest)
run (JobUrl jobUrl) pr = do
  start <- liftIO getCurrentTime

  flip withAnnotatedException (handleException start jobUrl) $ do
    env <- asks getJobEnv
    when env.repoDisabled $ throwIO RepoDisabled
    for_ env.planRestriction $ throwIO . flip PlanUpgradeRequired env.planUpgradeUrl

    clonePullRequest pr
    GHA.run pr.repo pr.number `with` \case
      RestyleSkipped config pullRequest reason -> do
        cleanupRestyledPullRequest pullRequest
        setPullRequestGreen config pullRequest jobUrl
          $ "Skipped ("
          <> renderSkipped reason
          <> ")"
        recordDoneStats start $ Right ()
      RestyleSuccessNoDifference config pullRequest _ -> do
        cleanupRestyledPullRequest pullRequest
        setPullRequestGreen config pullRequest jobUrl "No differences"
        recordDoneStats start $ Right ()
      RestyleSuccessDifference config pullRequest results -> do
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

        let mDetails = checkDangerousPullRequest pullRequest

        statusUrl <- case (cPullRequests config, mDetails) of
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
              Nothing -> createRestyledPullRequest config pullRequest results
              Just rpr -> updateRestyledPullRequest pullRequest rpr results

        setPullRequestRed config pullRequest statusUrl "Restyling produced differences"
        recordDoneStats start $ Right ()

handleException
  :: (MonadIO m, MonadGitHub m, MonadReader env m, HasStatsClient env)
  => UTCTime
  -> URL
  -> AnnotatedException SomeException
  -> m ()
handleException start jobUrl aex = do
  for_ mPullRequest $ \pullRequest -> do
    cleanupRestyledPullRequest pullRequest
    for_ mConfig $ \config -> do
      setPullRequestRed config pullRequest jobUrl
        $ "Error ("
        <> errorMetadataDescription md
        <> ")"
  recordDoneStats start $ Left md
 where
  ex = unannotatedException aex
  md = errorMetadata ex
  mConfig = findAnnotation @Config aex
  mPullRequest = findAnnotation @PullRequest aex

cleanupRestyledPullRequest :: MonadGitHub m => PullRequest -> m ()
cleanupRestyledPullRequest pullRequest = do
  mRestyledPullRequest <- findRestyledPullRequest pullRequest
  traverse_ closeRestyledPullRequest mRestyledPullRequest

setPullRequestGreen
  :: (MonadIO m, MonadGitHub m) => Config -> PullRequest -> URL -> Text -> m ()
setPullRequestGreen _config pullRequest =
  setPullRequestStatus pullRequest CommitStatusSuccess

setPullRequestRed
  :: (MonadIO m, MonadGitHub m) => Config -> PullRequest -> URL -> Text -> m ()
setPullRequestRed _config pullRequest =
  setPullRequestStatus pullRequest CommitStatusError

recordDoneStats
  :: (MonadIO m, MonadReader env m, HasStatsClient env)
  => UTCTime
  -> Either ErrorMetadata ()
  -> m ()
recordDoneStats start emd = do
  case emd of
    Left md -> Statsd.increment "restyler.error" $ errorMetadataStatsdTags md
    Right () -> Statsd.increment "restyler.success" []
  Statsd.increment "restyler.finished" []
  Statsd.histogramSince "restyler.duration" [] start
