module Restyler.Exit
  ( withExitHandler
  ) where

import Restyler.Prelude

import Data.Time (UTCTime, getCurrentTime)
import Restyler.ErrorMetadata
import Restyler.GitHub.Api
import Restyler.GitHub.Commit.Status
import Restyler.Options.JobUrl
import Restyler.Options.PullRequest
import Restyler.Statsd (HasStatsClient (..))
import Restyler.Statsd qualified as Statsd
import UnliftIO.Exception (tryAny)

withExitHandler
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadGitHub m
     , MonadReader env m
     , HasStatsClient env
     )
  => JobUrl
  -> PullRequestOption
  -> m a
  -> m ()
withExitHandler jobUrl pr f = do
  start <- liftIO getCurrentTime
  result <- tryAny f
  handleResult jobUrl pr result `finally` recordDoneStats start

handleResult
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadGitHub m
     , MonadReader env m
     , HasStatsClient env
     )
  => JobUrl
  -> PullRequestOption
  -> Either SomeException a
  -> m ()
handleResult jobUrl pr = \case
  Left ex | notExitSuccess ex -> do
    let md = errorMetadata ex
    recordErrorStat md
    errorPullRequest jobUrl pr md
  _ -> recordSuccessStat

notExitSuccess :: SomeException -> Bool
notExitSuccess = (Just ExitSuccess /=) . fromException

errorPullRequest
  :: (MonadIO m, MonadGitHub m)
  => JobUrl
  -> PullRequestOption
  -> ErrorMetadata
  -> m ()
errorPullRequest jobUrl pr md = do
  pullRequest <- getPullRequest pr.repo pr.number
  setPullRequestStatus pullRequest CommitStatusError jobUrl.unwrap
    $ "Error ("
    <> errorMetadataDescription md
    <> ")"

recordSuccessStat
  :: (MonadIO m, MonadReader env m, HasStatsClient env) => m ()
recordSuccessStat = Statsd.increment "restyler.success" []

recordErrorStat
  :: (MonadIO m, MonadReader env m, HasStatsClient env) => ErrorMetadata -> m ()
recordErrorStat md =
  Statsd.increment "restyler.error" $ errorMetadataStatsdTags md

recordDoneStats
  :: (MonadIO m, MonadReader env m, HasStatsClient env) => UTCTime -> m ()
recordDoneStats start = do
  Statsd.increment "restyler.finished" []
  Statsd.histogramSince "restyler.duration" [] start
