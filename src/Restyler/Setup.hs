{-# LANGUAGE LambdaCase #-}

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
import Restyler.PullRequest.Restyled
import Restyler.PullRequestSpec

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
    => RIO env (PullRequest, Maybe SimplePullRequest, Config)
restylerSetup = do
    Options {..} <- view optionsL

    logInfo "Restyler starting"
    pullRequest <-
        mapAppError toPullRequestFetchError $ runGitHub $ pullRequestR
            oOwner
            oRepo
            oPullRequest

    mRestyledPullRequest <-
        handleAny (const $ pure Nothing)
        $ runGitHubFirst
        $ pullRequestsForR oOwner oRepo
        $ pullRequestRestyledMod pullRequest

    when (pullRequestIsClosed pullRequest) $ do
        closeRestyledPullRequest' pullRequest mRestyledPullRequest
        exitWithInfo "Source Pull Request is closed"

    logInfo "Cloning repository"
    setupClone pullRequest

    config <- mapAppError ConfigurationError loadConfig
    unless (cEnabled config) $ exitWithInfo "Restyler disabled by config"

    labels <- getPullRequestLabelNames pullRequest
    when (labels `intersects` cIgnoreLabels config)
        $ exitWithInfo "Ignoring PR based on its labels"

    logInfo $ "Restyling " <> displayShow (pullRequestSpec pullRequest)
    logInfo $ displayRestyled pullRequest mRestyledPullRequest
    logDebug $ displayConfig config

    pure (pullRequest, mRestyledPullRequest, config)

displayConfig :: Config -> Utf8Builder
displayConfig =
    fromString
        . unpack
        . ("Resolved configuration\n" <>)
        . decodeUtf8
        . Yaml.encode

displayRestyled :: PullRequest -> Maybe SimplePullRequest -> Utf8Builder
displayRestyled pr = \case
    Nothing -> "Restyled PR does not exist"
    Just rpr -> "Restyled PR is " <> displayShow (pullRequestSpec pr)
        { prsPullRequest = simplePullRequestNumber rpr
        }

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
    gitCheckout $ unpack $ pullRequestRestyledRef pullRequest

toPullRequestFetchError :: AppError -> AppError
toPullRequestFetchError (GitHubError _ e) = PullRequestFetchError e
toPullRequestFetchError e = e

toPullRequestCloneError :: AppError -> AppError
toPullRequestCloneError (SystemError e) = PullRequestCloneError e
toPullRequestCloneError e = e
