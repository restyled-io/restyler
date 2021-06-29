module Restyler.Statsd
    (
    -- * Setup
      HasStatsClient(..)
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
    , timed
    ) where

import Restyler.Prelude

import Network.StatsD.Datadog
    ( DogStatsSettings(..)
    , Metric
    , MetricName(..)
    , MetricType(..)
    , StatsClient
    , ToMetricValue
    , defaultSettings
    , withDogStatsD
    )
import qualified Network.StatsD.Datadog as DD
import RIO.Time (diffUTCTime, getCurrentTime)

withStatsClient
    :: MonadUnliftIO m => String -> Int -> (StatsClient -> m a) -> m a
withStatsClient host port f = do
    withDogStatsD settings f
  where
    settings = defaultSettings
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
--
wrap
    :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
    => Text -- ^ Metrics prefix
    -> [(Text, Text)] -- ^ Metrics Tags
    -> Int -- ^ Timeout in seconds
    -> m a -- ^ Action
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
timeoutWithMetric prefix tags timeoutSeconds = fmap hush . race
    (do
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

-- | Time an operation in seconds
timed
    :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
    => Text
    -> [(Text, Text)]
    -> m a
    -> m a
timed name tags f = do
    t <- liftIO getCurrentTime
    f `finally` capture t
  where
    capture t = do
        seconds <- (`diffUTCTime` t) <$> liftIO getCurrentTime
        send (metric @Int name Histogram $ round @_ @Int seconds) tags

metric :: ToMetricValue a => Text -> MetricType -> a -> Metric
metric = DD.metric . MetricName

send
    :: (MonadIO m, MonadReader env m, HasStatsClient env)
    => Metric
    -> [(Text, Text)]
    -> m ()
send metric' tags = do
    client <- view statsClientL
    DD.send client $ metric' & DD.tags .~ ddTags
    where ddTags = map (uncurry DD.tag) tags
