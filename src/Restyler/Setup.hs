{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Setup
    ( restylerSetup
    ) where

import Restyler.Prelude

import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Restyler.App.Class
import Restyler.App.Type
import Restyler.Model.Config
import Restyler.Model.PullRequest
import Restyler.Options

restylerSetup :: (HasCallStack, MonadApp m) => App -> m App
restylerSetup app = do
    Options {..} <- asks appOptions

    pullRequest <-
        mapAppError toPullRequestFetchError
        $ runGitHub
        $ pullRequestR oOwner oRepo
        $ mkId Proxy oPullRequest

    setupClone pullRequest

    configExists <- doesFileExist configPath
    config <- if configExists
        then decodeConfig =<< readFile configPath
        else pure defaultConfig

    mRestyledPullRequest <- findRestyledPullRequest pullRequest
    pure $ finalizeApp app pullRequest mRestyledPullRequest config

setupClone :: (HasCallStack, MonadApp m) => PullRequest -> m ()
setupClone pullRequest = mapAppError toPullRequestCloneError $ do
    dir <- asks appWorkingDirectory
    token <- asks appAccessToken

    let cloneUrl = pullRequestCloneUrlToken token pullRequest
    callProcess "git" ["clone", unpack cloneUrl, dir]
    setCurrentDirectory dir

    when (pullRequestIsNonDefaultBranch pullRequest) $ fetchOrigin
        (pullRequestBaseRef pullRequest)
        (pullRequestBaseRef pullRequest)

    when (pullRequestIsFork pullRequest) $ fetchOrigin
        (pullRequestRemoteHeadRef pullRequest)
        (pullRequestLocalHeadRef pullRequest)

    callProcess "git" ["checkout", unpack $ pullRequestLocalHeadRef pullRequest]
  where
    fetchOrigin remoteRef localRef = callProcess
        "git"
        ["fetch", "origin", unpack $ remoteRef <> ":" <> localRef]

decodeConfig :: MonadApp m => Text -> m Config
decodeConfig =
    either (throwError . ConfigurationError) pure
        . Yaml.decodeEither
        . encodeUtf8

findRestyledPullRequest
    :: MonadApp m => PullRequest -> m (Maybe SimplePullRequest)
findRestyledPullRequest pullRequest = (V.!? 0) <$> runGitHub request
  where
    request = pullRequestsForR owner name options FetchAll
    options = optionsBase base <> optionsHead (toPathPart owner <> ":" <> head)
    owner = pullRequestOwnerName pullRequest
    name = pullRequestRepoName pullRequest
    base = pullRequestRestyledBase pullRequest
    head = pullRequestRestyledRef pullRequest

toPullRequestFetchError :: AppError -> AppError
toPullRequestFetchError (GitHubError e) = PullRequestFetchError e
toPullRequestFetchError e = e

toPullRequestCloneError :: AppError -> AppError
toPullRequestCloneError (SystemError e) = PullRequestCloneError e
toPullRequestCloneError (OtherError e) = PullRequestCloneError e
toPullRequestCloneError e = e
