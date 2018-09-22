{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.App.Run
    ( runApp
    , bootstrapApp
    )
where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import Restyler.App
import Restyler.Logger
import Restyler.Model.Config
import Restyler.Model.PullRequest
import Restyler.Model.PullRequestSpec
import Restyler.Options

-- | Run an @'AppT'@ action for real
--
-- N.B. This unwraps only as far as @'ExceptT'@ so that it can be composed
-- together with @'bootstrapApp'@ and receive overall error-handling after.
--
-- See @"Restyler.Main"@.
--
runApp :: MonadIO m => App -> AppT m a -> ExceptT AppError m a
runApp app = runAppLoggingT app . flip runReaderT app . runAppT

-- | Bootstrap the initial @'App'@ type
--
-- We want to have the @'PullRequest'@ and @'Config'@ in our application
-- environment, so we need them to construct an @'App'@. However, it's valuable
-- to use our normal @'AppT'@ instances to interact with GitHub or call
-- processes so we get the same logging and error-handling there.
--
-- So this function uses a partial @'App'@ value and @'runApp'@ to build the
-- rest of it.
--
-- If the actions given to @'runApp'@ here try to access @'appPullRequest'@ or
-- @'appConfig'@, they will fail. So it's important those actions aren't
-- refactored away from this module, where that assumption is less obvious.
--
bootstrapApp :: HasCallStack => Options -> FilePath -> ExceptT AppError IO App
bootstrapApp Options {..} path = runApp app $ do
    pullRequest <- if oFake
        then pure $ error $ unlines
            [ "\n\nAborting because --fake was given and you reached a point"
            , "where an actual PullRequest was needed"
            ]
        else setupPullRequest path oOwner oRepo oPullRequest

    mRestyledPullRequest <- if oFake
        then pure Nothing
        else loadRestyledPullRequest pullRequest

    config <- loadConfig
    logDebugN $ "Parsed configuration:\n" <> decodeUtf8 (Yaml.encode config)

    pure app
        { appPullRequest = pullRequest
        , appConfig = config
        , appRestyledPullRequest = mRestyledPullRequest
        }
  where
    app :: App
    app = App
        { appLogLevel = oLogLevel
        , appLogColor = oLogColor
        , appAccessToken = oAccessToken
        , appPullRequest = error "Bootstrap appPullRequest forced"
        , appConfig = error "Bootstrap appConfig forced"
        , appRestyledPullRequest = Nothing
        }

setupPullRequest
    :: (HasCallStack, MonadIO m)
    => FilePath
    -> Name Owner
    -> Name Repo
    -> Int
    -> AppT m PullRequest
setupPullRequest path owner repo num = do
    pullRequest <-
        mapAppError toPullRequestFetchError $ getPullRequest owner repo $ mkId
            Proxy
            num

    setupClone pullRequest path

    let spec = pullRequestSpec pullRequest
    logInfoN $ "Restyling PR " <> showSpec spec
    pure pullRequest

setupClone :: (HasCallStack, MonadIO m) => PullRequest -> FilePath -> AppT m ()
setupClone pullRequest dir = mapAppError toPullRequestCloneError $ do
    token <- asks appAccessToken

    cloneRepository (cloneUrl token) dir
    setCurrentDirectory dir

    when (pullRequestIsNonDefaultBranch pullRequest) $ fetchOrigin
        (pullRequestBaseRef pullRequest)
        (pullRequestBaseRef pullRequest)

    when (pullRequestIsFork pullRequest) $ fetchOrigin
        (pullRequestRemoteHeadRef pullRequest)
        (pullRequestLocalHeadRef pullRequest)

    checkoutBranch $ pullRequestLocalHeadRef pullRequest
  where
    cloneUrl token =
        "https://x-access-token:"
            <> token
            <> "@github.com/"
            <> untagName (pullRequestOwnerName pullRequest)
            <> "/"
            <> untagName (pullRequestRepoName pullRequest)
            <> ".git"

loadRestyledPullRequest
    :: (HasCallStack, MonadIO m)
    => PullRequest
    -> AppT m (Maybe SimplePullRequest)
loadRestyledPullRequest pullRequest = do
    mRestyledPullRequest <- findPullRequest
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestRestyledBase pullRequest)
        headParameter

    for mRestyledPullRequest $ \restyledPullRequest -> do
        let
            spec = PullRequestSpec
                { prsOwner = pullRequestOwnerName pullRequest
                , prsRepo = pullRequestRepoName pullRequest
                , prsPullRequest = simplePullRequestNumber restyledPullRequest
                }
        logInfoN $ "Existing restyled PR: " <> showSpec spec
        pure restyledPullRequest
  where
    -- N.B. we're assuming here that we'll never solve the problem of opening
    -- the Restyled PR in the forked repository, so the argument for the head
    -- parameter (user:branch) will always have the source repository owner as
    -- the user. Surprisingly, if this parameter is invalid (e.g. no user: part)
    -- it is ignored, and we just end up thinking the first PR in the repo is
    -- the Restyled PR!
    headParameter =
        toPathPart (pullRequestOwnerName pullRequest)
            <> ":"
            <> pullRequestRestyledRef pullRequest

-- | Load @.restyled.yaml@
loadConfig :: MonadIO m => AppT m Config
loadConfig = do
    exists <- doesFileExist configPath

    if exists then decodeConfig =<< readFile configPath else pure defaultConfig

-- | Decode the configuration content, or throw an @'AppError'@
decodeConfig :: Monad m => Text -> AppT m Config
decodeConfig =
    either (throwError . ConfigurationError) pure
        . Yaml.decodeEither
        . encodeUtf8

toPullRequestFetchError :: AppError -> AppError
toPullRequestFetchError (GitHubError e) = PullRequestFetchError e
toPullRequestFetchError e = e

toPullRequestCloneError :: AppError -> AppError
toPullRequestCloneError (SystemError e) = PullRequestCloneError e
toPullRequestCloneError (OtherError e) = PullRequestCloneError e
toPullRequestCloneError e = e

cloneRepository :: MonadIO m => Text -> FilePath -> AppT m ()
cloneRepository url dir = callProcess "git" ["clone", unpack url, dir]

fetchOrigin :: MonadIO m => Text -> Text -> AppT m ()
fetchOrigin remoteRef localRef =
    callProcess "git" ["fetch", "origin", unpack $ remoteRef <> ":" <> localRef]

checkoutBranch :: MonadIO m => Text -> AppT m ()
checkoutBranch branch = callProcess "git" ["checkout", unpack branch]
