{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.App.Run
    ( bootstrapApp
    )
where

import Restyler.Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Yaml as Yaml
import GitHub
import Restyler.Git
import Restyler.GitHub
import Restyler.Options
import Restyler.PullRequest
import Restyler.RepoSpec

-- | Bootstrap the initial @'App'@ type
--
-- We want to have the @'PullRequest'@ and @'Config'@ in our application
-- environment, so we need them to construct an @'App'@. However, it's valuable
-- to use our normal @'AppM'@ actions to interact with GitHub or call processes
-- so we get the same logging and error-handling there.
--
-- So this function builds a partial @'App'@ type, then uses it to run @'AppM'@
-- actions to add the rest of the data before returning it.
--
bootstrapApp :: Options -> FilePath -> ExceptT AppError IO App
bootstrapApp Options {..} path = runApp app $ do
    pullRequest <-
        mapAppError toPullRequestFetchError
        $ runGitHub
        $ pullRequestR oOwner oRepo
        $ mkId Proxy oPullRequest

    setupClone pullRequest path

    let spec = pullRequestRepoSpec pullRequest
    logInfoN $ "Restyling PR " <> showRepoSpec spec

    config <- loadConfig
    logDebugN $ "Loaded config: " <> tshow config

    pure app { appPullRequest = pullRequest, appConfig = config }
  where
    app :: App
    app = App
        { appLogLevel = oLogLevel
        , appAccessToken = oAccessToken
        , appPullRequest = error "Bootstrap appPullRequest forced"
        , appConfig = error "Bootstrap appConfig forced"
        }

setupClone :: PullRequest -> FilePath -> AppM ()
setupClone pullRequest dir = mapAppError toPullRequestCloneError $ do
    token <- asks appAccessToken

    cloneRepository (cloneUrl token) dir
    setCurrentDirectory dir

    when (pullRequestIsFork pullRequest) $ fetchOrigin
        (pullRequestRemoteHeadRef pullRequest)
        (pullRequestLocalHeadRef pullRequest)

    checkoutBranch False $ pullRequestLocalHeadRef pullRequest
  where
    cloneUrl token =
        "https://x-access-token:"
            <> token
            <> "@github.com/"
            <> untagName (pullRequestOwnerName pullRequest)
            <> "/"
            <> untagName (pullRequestRepoName pullRequest)
            <> ".git"

-- | Load @.restyled.yaml@
loadConfig :: AppM Config
loadConfig = do
    exists <- doesFileExist configPath

    if exists
        then decodeConfig =<< liftIOApp (BS.readFile configPath)
        else pure defaultConfig

-- | Decode the configuration content, or throw an @'AppError'@
decodeConfig :: ByteString -> AppM Config
decodeConfig =
    either (throwError . ConfigurationError) pure . Yaml.decodeEither

toPullRequestFetchError :: AppError -> AppError
toPullRequestFetchError (GitHubError e) = PullRequestFetchError e
toPullRequestFetchError e = e

toPullRequestCloneError :: AppError -> AppError
toPullRequestCloneError (OtherError e) = PullRequestCloneError e
toPullRequestCloneError e = e
