module Restyler.Setup
    ( restylerSetup

    -- * Errors
    , CloneTimeoutError(..)
    ) where

import Restyler.Prelude

import GitHub.Endpoints.PullRequests
import Restyler.App.Class
import Restyler.Config
import Restyler.Git
import Restyler.Ignore
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequest.Status
import Restyler.RestyledPullRequest
import qualified Restyler.Statsd as Statsd
import Restyler.Statsd (HasStatsClient)

restylerSetup
    :: ( HasCallStack
       , MonadUnliftIO m
       , MonadLogger m
       , MonadSystem m
       , MonadExit m
       , MonadProcess m
       , MonadGitHub m
       , MonadDownloadFile m
       , MonadReader env m
       , HasOptions env
       , HasWorkingDirectory env
       , HasStatsClient env
       )
    => m (PullRequest, Config)
restylerSetup = do
    Options {..} <- view optionsL

    logInfo "Restyler starting"
    pullRequest <- runGitHub $ pullRequestR oOwner oRepo oPullRequest

    logInfo $ "Restyling PR" :# ["number" .= pullRequestNumber pullRequest]

    when (pullRequestIsClosed pullRequest) $ do
        mRestyledPullRequest <- findRestyledPullRequest pullRequest
        traverse_ closeRestyledPullRequest mRestyledPullRequest
        exitWithInfo "Source Pull Request is closed"

    logInfo "Cloning repository"
    wrapClone $ setupClone pullRequest

    config <- loadConfig
    logDebug $ "Resolved configuration" :# ["config" .= config]
    unless (cEnabled config) $ exitWithInfo "Restyler disabled by config"

    mIgnoredReason <- getIgnoredReason config pullRequest
    for_ mIgnoredReason $ \reason -> do
        let item = case reason of
                IgnoredByAuthor{} -> "author"
                IgnoredByBranch{} -> "branch"
                IgnoredByLabels{} -> "labels"
            status = SkippedStatus ("Ignore " <> item) oJobUrl
        sendPullRequestStatus' config pullRequest status
        exitWithInfo $ "Ignoring PR" :# ["reason" .= show @Text reason]

    pure (pullRequest, config)

setupClone
    :: ( HasCallStack
       , MonadSystem m
       , MonadProcess m
       , MonadReader env m
       , HasOptions env
       , HasWorkingDirectory env
       )
    => PullRequest
    -> m ()
setupClone pullRequest = do
    dir <- view workingDirectoryL
    token <- oAccessToken <$> view optionsL
    gitCloneBranchByRef
        (unpack $ pullRequestRemoteHeadRef pullRequest)
        (unpack $ pullRequestLocalHeadRef pullRequest)
        (unpack $ pullRequestCloneUrlToken token pullRequest)
        dir

newtype CloneTimeoutError = CloneTimeoutError
    { cloneTimeoutDurationMinutes :: Int
    }
    deriving stock Show

instance Exception CloneTimeoutError where
    displayException ex =
        "Clone timed out after "
            <> show @String (cloneTimeoutDurationMinutes ex)
            <> " minutes"

wrapClone
    :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env) => m a -> m ()
wrapClone f = do
    mResult <- Statsd.wrap "restyler.clone" [] (30 * 60) f
    when (isNothing mResult) $ throwIO timedOutError
    where timedOutError = CloneTimeoutError 30
