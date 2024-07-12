{-# LANGUAGE NamedFieldPuns #-}

module Restyler.Statsd
  ( -- * Setup
    HasStatsClient (..)
  , StatsClient
  , withStatsClient

    -- * Sending metrics

    -- ** Convenience
  , wrap
  , timeoutWithMetric
  , incrementOnException
  , incrementOnSuccess

    -- * Lower-level
  , increment
  , histogram
  , histogramSince
  , timed
  ) where

import Restyler.Prelude

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Network.StatsD.Datadog
  ( DogStatsSettings (..)
  , Metric
  , MetricName (..)
  , MetricType (..)
  , ToMetricValue
  , defaultSettings
  , withDogStatsD
  )
import Network.StatsD.Datadog qualified as DD

data StatsClient = StatsClient
  { statsClient :: DD.StatsClient
  , globalTags :: [(Text, Text)]
  }

withStatsClient
  :: MonadUnliftIO m
  => Maybe String
  -> Maybe Int
  -> [(Text, Text)]
  -> (StatsClient -> m a)
  -> m a
withStatsClient mHost mPort globalTags f = do
  case mSettings of
    Nothing -> f StatsClient {statsClient = DD.Dummy, globalTags}
    Just settings -> withDogStatsD settings
      $ \statsClient -> f StatsClient {statsClient, globalTags}
 where
  mSettings = case (mHost, mPort) of
    (Nothing, Nothing) -> Nothing
    (Just host, Nothing) ->
      Just defaultSettings {dogStatsSettingsHost = host}
    (Nothing, Just port) ->
      Just defaultSettings {dogStatsSettingsPort = port}
    (Just host, Just port) ->
      Just
        defaultSettings
          { dogStatsSettingsHost = host
          , dogStatsSettingsPort = port
          }

class HasStatsClient env where
  statsClientL :: Lens' env StatsClient

instance HasStatsClient StatsClient where
  statsClientL = id

-- | Generic wrapper for observing an operation
--
-- Adds success/failure metrics, and enforces a timeout
wrap
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Text
  -- ^ Metrics prefix
  -> [(Text, Text)]
  -- ^ Metrics Tags
  -> Int
  -- ^ Timeout in seconds
  -> m a
  -- ^ Action
  -> m (Maybe a)
wrap prefix tags timeoutSeconds =
  timed prefix tags
    . timeoutWithMetric prefix tags timeoutSeconds
    . incrementOnException prefix tags
    . incrementOnSuccess prefix tags

timeoutWithMetric
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> [(Text, Text)]
  -> Int
  -> m a
  -> m (Maybe a)
timeoutWithMetric prefix tags timeoutSeconds =
  fmap hush
    . race
      ( do
          threadDelay $ timeoutSeconds * 1000000
          increment (prefix <> ".timeout") tags
      )

incrementOnException
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> [(Text, Text)]
  -> m a
  -> m a
incrementOnException prefix tags =
  (`onException` increment (prefix <> ".failed") tags)

incrementOnSuccess
  :: (MonadIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> [(Text, Text)]
  -> m a
  -> m a
incrementOnSuccess prefix tags = (<* increment (prefix <> ".success") tags)

increment
  :: (MonadIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> [(Text, Text)]
  -> m ()
increment name = send $ metric @Int name Counter 1

histogram
  :: (MonadIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> [(Text, Text)]
  -> NominalDiffTime
  -> m ()
histogram name tags diff =
  send (metric name Histogram $ round @_ @Int diff) tags

histogramSince
  :: (MonadIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> [(Text, Text)]
  -> UTCTime
  -> m ()
histogramSince name tags t = do
  diff <- (`diffUTCTime` t) <$> liftIO getCurrentTime
  histogram name tags diff

-- | Time an operation in seconds
timed
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> [(Text, Text)]
  -> m a
  -> m a
timed name tags f = do
  t <- liftIO getCurrentTime
  f `finally` histogramSince name tags t

metric :: ToMetricValue a => Text -> MetricType -> a -> Metric
metric = DD.metric . MetricName

send
  :: (MonadIO m, MonadReader env m, HasStatsClient env)
  => Metric
  -> [(Text, Text)]
  -> m ()
send metric' tags = do
  StatsClient {statsClient, globalTags} <- view statsClientL
  let ddTags = map (uncurry DD.tag) $ globalTags <> tags
  DD.send statsClient $ metric' & DD.tags .~ ddTags
