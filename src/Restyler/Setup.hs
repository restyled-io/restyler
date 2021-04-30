

module Restyler.Setup
    ( restylerSetup
    )
where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import GitHub.Endpoints.PullRequests
import Restyler.App.Class
import Restyler.App.Error
import Restyler.Config
import Restyler.Git
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequest.Status
import Restyler.RestyledPullRequest

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

    mRestyledPullRequest <- findRestyledPullRequest pullRequest

    when (pullRequestIsClosed pullRequest) $ do
        traverse_ closeRestyledPullRequest mRestyledPullRequest
        exitWithInfo "Source Pull Request is closed"

    logInfo "Cloning repository"
    setupClone pullRequest

    config <- mapAppError ConfigurationError loadConfig
    unless (cEnabled config) $ exitWithInfo "Restyler disabled by config"

    labels <- getPullRequestLabelNames pullRequest
    when (labels `intersects` cIgnoreLabels config) $ do
        let status = SkippedStatus "ignore labels" oJobUrl
        sendPullRequestStatus' config pullRequest status
        exitWithInfo "Ignoring PR based on its labels"

    case mRestyledPullRequest of
        Nothing -> do
            logInfo "No existing Restyled PR"
            gitCheckout $ unpack $ pullRequestRestyledHeadRef pullRequest
        Just pr -> do
            logInfo $ "Existing Restyled PR is " <> display pr
            gitCheckout $ unpack $ restyledPullRequestHeadRef pr

    logInfo $ "Restyling " <> display pullRequest
    logDebug $ displayConfigYaml config
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

toPullRequestFetchError :: AppError -> AppError
toPullRequestFetchError (GitHubError _ e) = PullRequestFetchError e
toPullRequestFetchError e = e

toPullRequestCloneError :: AppError -> AppError
toPullRequestCloneError (SystemError e) = PullRequestCloneError e
toPullRequestCloneError e = e
