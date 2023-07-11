module Restyler.Exit
  ( withExitHandler
  ) where

import Restyler.Prelude

import Data.Time (UTCTime, getCurrentTime)
import GitHub.Endpoints.PullRequests
import GitHub.Endpoints.Repos.Statuses
import Lens.Micro (_1, _2, _3)
import Restyler.App (runGitHubInternal)
import Restyler.ErrorMetadata
import Restyler.Options
import Restyler.PullRequest
import Restyler.Statsd (HasStatsClient (..), StatsClient)
import qualified Restyler.Statsd as Statsd
import UnliftIO.Exception (tryAny)

newtype ExitHandler = ExitHandler
  { unExitHandler :: (Logger, StatsClient, Options)
  }

unL :: Lens' ExitHandler (Logger, StatsClient, Options)
unL = lens unExitHandler $ \x y -> x {unExitHandler = y}

instance HasLogger ExitHandler where
  loggerL = unL . _1 . loggerL

instance HasStatsClient ExitHandler where
  statsClientL = unL . _2 . statsClientL

instance HasOptions ExitHandler where
  optionsL = unL . _3 . optionsL

runExitHandler
  :: MonadUnliftIO m
  => Logger
  -> StatsClient
  -> Options
  -> ReaderT ExitHandler (LoggingT m) a
  -> m a
runExitHandler logger statsClient options =
  runLoggerLoggingT logger . flip runReaderT env
 where
  env = ExitHandler (logger, statsClient, options)

withExitHandler
  :: MonadUnliftIO m => Logger -> StatsClient -> Options -> m () -> m ExitCode
withExitHandler logger statsClient options f = do
  start <- liftIO getCurrentTime
  result <- tryAny f
  runExitHandler logger statsClient options
    $ handleResult result
    `finally` recordDoneStats start

handleResult
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasStatsClient env
     , HasOptions env
     )
  => Either SomeException ()
  -> m ExitCode
handleResult = \case
  Left ex
    | isExitSuccess ex ->
        ExitSuccess <$ Statsd.increment "restyler.success" []
  Left ex -> do
    let md = errorMetadata ex

    Statsd.increment "restyler.error" $ errorMetadataStatsdTags md

    logError
      $ ("Exception:\n" <> pack (displayException ex))
      :# ["error" .= md]

    errorMetadataExitCode md
      <$ errorPullRequest (errorMetadataDescription md)
  Right () -> ExitSuccess <$ Statsd.increment "restyler.success" []

-- TODO: stop using exitSuccess for flow-control
isExitSuccess :: SomeException -> Bool
isExitSuccess = maybe False (== ExitSuccess) . fromException

errorPullRequest
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasOptions env)
  => Text
  -> m ()
errorPullRequest description = warnIgnore $ do
  Options {..} <- view optionsL
  pr <- runGitHubInternal $ pullRequestR oOwner oRepo oPullRequest

  let
    sha = mkName Proxy $ pullRequestHeadSha pr
    status =
      NewStatus
        { newStatusState = StatusError
        , newStatusTargetUrl = oJobUrl
        , newStatusDescription = Just $ "Error (" <> description <> ")"
        , newStatusContext = Just "restyled"
        }

  void $ runGitHubInternal $ createStatusR oOwner oRepo sha status

recordDoneStats
  :: (MonadIO m, MonadReader env m, HasStatsClient env) => UTCTime -> m ()
recordDoneStats start = do
  Statsd.increment "restyler.finished" []
  Statsd.histogramSince "restyler.duration" [] start
