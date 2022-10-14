module Restyler.Exit
    ( withExitHandler
    ) where

import Restyler.Prelude

import Data.Time (UTCTime, getCurrentTime)
import GitHub.Endpoints.PullRequests
import GitHub.Endpoints.Repos.Statuses
import Lens.Micro (_1, _2, _3)
import Restyler.App (GitHubError(..), runGitHubInternal)
import Restyler.Config (ConfigError(..))
import Restyler.Options
import Restyler.PullRequest
import Restyler.Restyler.Run (RestylerError(..))
import Restyler.Statsd (HasStatsClient(..), StatsClient)
import qualified Restyler.Statsd as Statsd
import UnliftIO.Exception (tryAny)

newtype ExitHandler = ExitHandler
    { unExitHandler :: (Logger, StatsClient, Options)
    }

unL :: Lens' ExitHandler (Logger, StatsClient, Options)
unL = lens unExitHandler $ \x y -> x { unExitHandler = y }

instance HasLogger ExitHandler where
    loggerL = unL . _1 . loggerL

instance HasStatsClient ExitHandler where
    statsClientL = unL . _2 . statsClientL

instance HasOptions ExitHandler where
    optionsL = unL . _3 . optionsL

runExitHandler
    :: MonadIO m
    => Logger
    -> StatsClient
    -> Options
    -> ReaderT ExitHandler (LoggingT m) a
    -> m a
runExitHandler logger statsClient options =
    runLoggerLoggingT logger . flip runReaderT env
    where env = ExitHandler (logger, statsClient, options)

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
    Left ex | isExitSuccess ex -> pure ExitSuccess
    Left ex -> do
        let (severity, errorTag, exitCode) = errorMetadata ex

        Statsd.increment
            "restyler.error"
            [("severity", severity), ("error", errorTag)]

        logError
            $ ("Exception:\n" <> pack (displayException ex))
            :# ["severity" .= severity, "error" .= errorTag]

        -- We _should_ check config.statuses here, but we may not have been able
        -- to even clone, let alone read Config. And doing it in cases we can
        -- requires going back to multi-layered error-handling. Ugh.
        ExitFailure exitCode <$ errorPullRequest

    Right () -> ExitSuccess <$ Statsd.increment "restyler.success" []

-- TODO: stop using exitSuccess for flow-control
isExitSuccess :: SomeException -> Bool
isExitSuccess = maybe False (== ExitSuccess) . fromException

type ErrorMetadata = (Text, Text, Int)

errorMetadata :: SomeException -> ErrorMetadata
errorMetadata = fromMaybe unknown . getFirst . fold . handlers

handlers :: SomeException -> [First ErrorMetadata]
handlers e =
    [ fromException e & First <&> \case
        ConfigErrorInvalidYaml{} -> ("warning", "invalid-config", 10)
        ConfigErrorInvalidRestylers{} ->
            ("warning", "invalid-config-restylers", 11)
        ConfigErrorInvalidRestylersYaml{} ->
            ("error", "invalid-restylers-yaml", 12)
    , fromException e & First <&> \case
        RestyleError{} -> ("warning", "restyle-error", 25)
        RestylerExitFailure{} -> ("warning", "restyler", 20)
    , fromException e & First <&> \case
        GitHubError{} -> ("warning", "github", 30)
    ]

unknown :: ErrorMetadata
unknown = ("critical", "unknown", 99)

errorPullRequest
    :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasOptions env)
    => m ()
errorPullRequest = warnIgnore $ do
    Options {..} <- view optionsL
    pr <- runGitHubInternal $ pullRequestR oOwner oRepo oPullRequest

    let sha = mkName Proxy $ pullRequestHeadSha pr
        status = NewStatus
            { newStatusState = StatusError
            , newStatusTargetUrl = oJobUrl
            , newStatusDescription = Just "Error restyling Pull Request"
            , newStatusContext = Just "restyled"
            }

    void $ runGitHubInternal $ createStatusR oOwner oRepo sha status

recordDoneStats
    :: (MonadIO m, MonadReader env m, HasStatsClient env) => UTCTime -> m ()
recordDoneStats start = do
    Statsd.increment "restyler.finished" []
    Statsd.histogramSince "restyler.duration" [] start
