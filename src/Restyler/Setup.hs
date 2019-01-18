{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Setup
    ( restylerSetup
    ) where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import Restyler.App
import Restyler.Config
import Restyler.Options
import Restyler.PullRequest

restylerSetup
    :: (HasCallStack, MonadApp m)
    => m (PullRequest, Maybe SimplePullRequest, Config)
restylerSetup = do
    Options {..} <- asks appOptions

    pullRequest <-
        mapAppError toPullRequestFetchError
        $ runGitHub
        $ pullRequestR oOwner oRepo
        $ mkId Proxy oPullRequest

    mRestyledPullRequest <-
        (`catchError` const (pure Nothing))
        $ runGitHubFirst
        $ pullRequestsForR oOwner oRepo
        $ pullRequestRestyledMod pullRequest

    setupClone pullRequest

    configExists <- doesFileExist configPath
    config <- if configExists
        then decodeConfig =<< readFile configPath
        else pure defaultConfig

    pure (pullRequest, mRestyledPullRequest, config)

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
        . Yaml.decodeEither'
        . encodeUtf8

toPullRequestFetchError :: AppError -> AppError
toPullRequestFetchError (GitHubError e) = PullRequestFetchError e
toPullRequestFetchError e = e

toPullRequestCloneError :: AppError -> AppError
toPullRequestCloneError (SystemError e) = PullRequestCloneError e
toPullRequestCloneError (OtherError e) = PullRequestCloneError e
toPullRequestCloneError e = e
