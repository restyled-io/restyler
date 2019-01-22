{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE RecordWildCards #-}

module Restyler.Setup
    ( restylerSetup
    ) where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import Restyler.App
import Restyler.Config
import Restyler.Git
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
