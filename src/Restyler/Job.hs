module Restyler.Job
  ( run
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Restyler.AnnotatedException
import Restyler.App.Class (MonadDownloadFile, MonadSystem)
import Restyler.Clone
import Restyler.Config
import Restyler.DangerousPullRequest
import Restyler.Docker (MonadDocker)
import Restyler.GHA qualified as GHA
import Restyler.GHA.Output
import Restyler.Git
import Restyler.GitHub.Api
import Restyler.GitHub.Commit.Status
import Restyler.GitHub.PullRequest
import Restyler.Job.AgentEnv
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
import Restyler.RestylerResult
import Restyler.Statsd (HasStatsClient)
import Restyler.Statsd qualified as Statsd

run
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadGitHub m
     , MonadGit m
     , MonadDocker m
     , MonadDownloadFile m
     , MonadReader env m
     , HasStatsClient env
     , HasAgentEnv env
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
  assertAgentEnv

  withStatsAndCleanup jobUrl $ do
    clonePullRequest pr

    GHA.run pr.repo pr.number `with` \case
      RestyleSkipped config pullRequest reason -> do
        cleanupRestyledPullRequest pullRequest
        setPullRequestGreen config pullRequest jobUrl
          $ "Skipped ("
          <> renderSkipped reason
          <> ")"
      RestyleSuccessNoDifference config pullRequest _ -> do
        cleanupRestyledPullRequest pullRequest
        setPullRequestGreen config pullRequest jobUrl "No differences"
      RestyleSuccessDifference config pullRequest results -> do
        logGitPatch jobUrl pullRequest
        statusUrl <- handleDifferences config jobUrl pullRequest results
        setPullRequestRed config pullRequest statusUrl "Restyling produced differences"

logGitPatch
  :: (MonadMask m, MonadIO m, MonadLogger m, MonadGit m)
  => URL
  -> PullRequest
  -> m ()
logGitPatch jobUrl pullRequest = do
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

handleDifferences
  :: (MonadUnliftIO m, MonadLogger m, MonadGitHub m, MonadGit m)
  => Config
  -> URL
  -> PullRequest
  -> [RestylerResult]
  -> m URL
handleDifferences config jobUrl pullRequest results = do
  case (cPullRequests config, mDetails) of
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
 where
  mDetails = checkDangerousPullRequest pullRequest

withStatsAndCleanup
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadGitHub m
     , MonadReader env m
     , HasStatsClient env
     )
  => URL
  -> m a
  -> m a
withStatsAndCleanup jobUrl action = do
  start <- liftIO getCurrentTime
  flip (withAnnotatedException @SomeException) (cleanup start) $ do
    result <- action
    result <$ recordDoneStats start (Right ())
 where
  cleanup start aex = do
    recordDoneStats start $ Left aex

    let
      mConfig = findAnnotation @Config aex
      mPullRequest = findAnnotation @PullRequest aex

    for_ mPullRequest $ \pullRequest -> do
      for_ mConfig $ \config -> do
        suppressWarn
          $ setPullRequestRed config pullRequest jobUrl "Restyled has errored"

recordDoneStats
  :: (MonadIO m, MonadReader env m, HasStatsClient env)
  => UTCTime
  -> Either e ()
  -> m ()
recordDoneStats start result = do
  Statsd.increment "restyler.finished" []
  Statsd.histogramSince "restyler.duration" [] start
  case result of
    Left _ex -> Statsd.increment "restyler.error" []
    Right () -> Statsd.increment "restyler.success" []

cleanupRestyledPullRequest :: MonadGitHub m => PullRequest -> m ()
cleanupRestyledPullRequest pullRequest = do
  mRestyledPullRequest <- findRestyledPullRequest pullRequest
  traverse_ closeRestyledPullRequest mRestyledPullRequest

setPullRequestGreen
  :: (MonadUnliftIO m, MonadGitHub m) => Config -> PullRequest -> URL -> Text -> m ()
setPullRequestGreen _config pullRequest =
  setPullRequestStatus pullRequest CommitStatusSuccess

setPullRequestRed
  :: (MonadUnliftIO m, MonadGitHub m) => Config -> PullRequest -> URL -> Text -> m ()
setPullRequestRed _config pullRequest =
  setPullRequestStatus pullRequest CommitStatusError
