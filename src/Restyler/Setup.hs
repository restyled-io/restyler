module Restyler.Setup
    ( restylerSetup
    ) where

import Restyler.Prelude

import GitHub.Endpoints.PullRequests
import Prelude (userError)
import Restyler.App.Class
import Restyler.App.Error
import Restyler.Config
import Restyler.Git
import Restyler.Ignore
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequest.Status
import Restyler.RestyledPullRequest
import Restyler.Statsd (HasStatsClient)
import qualified Restyler.Statsd as Statsd

restylerSetup
    :: ( HasCallStack
       , MonadThrow m
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
    => m (PullRequest, Maybe RestyledPullRequest, Config)
restylerSetup = do
    Options {..} <- view optionsL

    logInfo "Restyler starting"
    pullRequest <-
        mapAppError toPullRequestFetchError $ runGitHub $ pullRequestR
            oOwner
            oRepo
            oPullRequest

    logInfo $ "Restyling PR" :# ["number" .= pullRequestNumber pullRequest]

    mRestyledPullRequest <- findRestyledPullRequest pullRequest

    logInfo $ maybe
        ("No existing Restyled PR" :# [])
        (\pr ->
            "Existing Restyled PR" :# ["number" .= restyledPullRequestNumber pr]
        )
        mRestyledPullRequest

    when (pullRequestIsClosed pullRequest) $ do
        traverse_ closeRestyledPullRequest mRestyledPullRequest
        exitWithInfo "Source Pull Request is closed"

    logInfo "Cloning repository"
    wrapClone $ setupClone pullRequest

    config <- mapAppError ConfigurationError loadConfig
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

    pure (pullRequest, mRestyledPullRequest, config)

setupClone
    :: ( HasCallStack
       , MonadUnliftIO m
       , MonadSystem m
       , MonadProcess m
       , MonadReader env m
       , HasOptions env
       , HasWorkingDirectory env
       )
    => PullRequest
    -> m ()
setupClone pullRequest = mapAppError toPullRequestCloneError $ do
    dir <- view workingDirectoryL
    token <- oAccessToken <$> view optionsL
    gitCloneBranchByRef
        (unpack $ pullRequestRemoteHeadRef pullRequest)
        (unpack $ pullRequestLocalHeadRef pullRequest)
        (unpack $ pullRequestCloneUrlToken token pullRequest)
        dir

wrapClone
    :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env) => m a -> m ()
wrapClone f = do
    mResult <- Statsd.wrap "restyler.clone" [] (30 * 60) f
    when (isNothing mResult) $ throwIO timedOutError
    where timedOutError = PullRequestCloneError $ userError "Timed out"

toPullRequestFetchError :: AppError -> AppError
toPullRequestFetchError (GitHubError _ e) = PullRequestFetchError e
toPullRequestFetchError e = e

toPullRequestCloneError :: AppError -> AppError
toPullRequestCloneError (SystemError e) = PullRequestCloneError e
toPullRequestCloneError e = e
