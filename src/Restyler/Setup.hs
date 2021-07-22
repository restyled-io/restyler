module Restyler.Setup
    ( restylerSetup
    ) where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
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
       , HasLogFunc env
       , HasOptions env
       , HasWorkingDirectory env
       , HasSystem env
       , HasExit env
       , HasProcess env
       , HasDownloadFile env
       , HasGitHub env
       , HasStatsClient env
       )
    => RIO env (PullRequest, Maybe RestyledPullRequest, Config)
restylerSetup = do
    Options {..} <- view optionsL

    logInfo "Restyler starting"
    pullRequest <-
        mapAppError toPullRequestFetchError $ runGitHub $ pullRequestR
            oOwner
            oRepo
            oPullRequest
    logInfo $ "Restyling " <> display pullRequest

    mRestyledPullRequest <- findRestyledPullRequest pullRequest
    logInfo $ maybe
        "No existing Restyled PR"
        (("Existing Restyled PR is " <>) . display)
        mRestyledPullRequest

    when (pullRequestIsClosed pullRequest) $ do
        traverse_ closeRestyledPullRequest mRestyledPullRequest
        exitWithInfo "Source Pull Request is closed"

    logInfo "Cloning repository"
    wrapClone $ setupClone pullRequest

    config <- mapAppError ConfigurationError loadConfig
    logDebug $ displayConfigYaml config
    unless (cEnabled config) $ exitWithInfo "Restyler disabled by config"

    mIgnoredReason <- getIgnoredReason config pullRequest
    for_ mIgnoredReason $ \reason -> do
        let item = case reason of
                IgnoredByAuthor -> "author"
                IgnoredByBranch -> "branch"
                IgnoredByLabels -> "labels"
            status = SkippedStatus ("Ignore " <> item) oJobUrl
        sendPullRequestStatus' config pullRequest status
        exitWithInfo $ "Ignoring PR based on its " <> display item

    case mRestyledPullRequest of
        Nothing ->
            gitCheckout $ unpack $ pullRequestRestyledHeadRef pullRequest
        Just pr -> gitCheckout $ unpack $ restyledPullRequestHeadRef pr

    pure (pullRequest, mRestyledPullRequest, config)

displayConfigYaml :: Config -> Utf8Builder
displayConfigYaml =
    fromString
        . unpack
        . ("Resolved configuration\n" <>)
        . decodeUtf8
        . Yaml.encode

setupClone
    :: ( HasCallStack
       , HasOptions env
       , HasWorkingDirectory env
       , HasSystem env
       , HasProcess env
       )
    => PullRequest
    -> RIO env ()
setupClone pullRequest = mapAppError toPullRequestCloneError $ do
    dir <- view workingDirectoryL
    token <- oAccessToken <$> view optionsL

    let cloneUrl = unpack $ pullRequestCloneUrlToken token pullRequest
    gitClone cloneUrl dir
    setCurrentDirectory dir

    when (pullRequestIsNonDefaultBranch pullRequest) $ gitFetch
        (unpack $ pullRequestBaseRef pullRequest)
        (unpack $ pullRequestBaseRef pullRequest)

    when (pullRequestIsFork pullRequest) $ gitFetch
        (unpack $ pullRequestRemoteHeadRef pullRequest)
        (unpack $ pullRequestLocalHeadRef pullRequest)

    gitCheckoutExisting $ unpack $ pullRequestLocalHeadRef pullRequest

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
