{-# LANGUAGE LambdaCase #-}

module Restyler.Setup
    ( restylerSetup
    )
where

import Restyler.Prelude

import Control.Monad.Except
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

    logInfo "Cloning repository"
    config <- eitherM (handleCloneFailure pullRequest) pure
        $ setupClone pullRequest

    (mRestyledPullRequest, restyledRef) <- findRestyled pullRequest config

    when (pullRequestIsClosed pullRequest) $ do
        closeRestyledPullRequest' pullRequest mRestyledPullRequest restyledRef
        exitWithInfo "Source Pull Request is closed"

    unless (cEnabled config) $ exitWithInfo "Restyler disabled by config"

    labels <- getPullRequestLabelNames pullRequest
    when (labels `intersects` cIgnoreLabels config)
        $ exitWithInfo "Ignoring PR based on its labels"

    logInfo $ "Restyling " <> displayShow (pullRequestSpec pullRequest)
    logInfo $ displayRestyled pullRequest mRestyledPullRequest
    logDebug $ displayConfig config

    gitCheckout $ unpack restyledRef
    pure (pullRequest, mRestyledPullRequest, config)

-- | TODO: incorporate the restyled ref in a new RestylePullRequest type
--
-- That way, we don't need to pass around tuples all the time, or read the
-- branch ref off of @'Config'@ repeatedly later.
--
--
findRestyled
    :: HasGitHub env
    => PullRequest
    -> Config
    -> RIO env (Maybe SimplePullRequest, Text)
findRestyled pullRequest config =
    fmap (, restyledRef)
        $ handleAny (const $ pure Nothing)
        $ runGitHubFirst
        $ pullRequestsForR
              (pullRequestOwnerName pullRequest)
              (pullRequestRepoName pullRequest)
        $ pullRequestRestyledMod pullRequest restyledRef
    where restyledRef = configRestyledRef pullRequest config

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

data CloneFailure = CloneFailure AppError (Maybe Config)

setupClone
    :: ( HasCallStack
       , HasLogFunc env
       , HasOptions env
       , HasWorkingDirectory env
       , HasSystem env
       , HasProcess env
       , HasDownloadFile env
       )
    => PullRequest
    -> RIO env (Either CloneFailure Config)
setupClone pullRequest = runExceptT $ do
    dir <- lift $ view workingDirectoryL
    token <- lift $ oAccessToken <$> view optionsL

    let cloneUrl = unpack $ pullRequestCloneUrlToken token pullRequest

    ExceptT $ tryToM (pure . (`CloneFailure` Nothing)) $ do
        gitClone cloneUrl dir
        setCurrentDirectory dir

    ExceptT
        $ tryToM cloneFailureWithConfig
        $ when (pullRequestIsNonDefaultBranch pullRequest)
        $ gitFetch
              (unpack $ pullRequestBaseRef pullRequest)
              (unpack $ pullRequestBaseRef pullRequest)

    ExceptT
        $ tryToM cloneFailureWithConfig
        $ when (pullRequestIsFork pullRequest)
        $ gitFetch
              (unpack $ pullRequestRemoteHeadRef pullRequest)
              (unpack $ pullRequestLocalHeadRef pullRequest)

    ExceptT
        $ tryToM cloneFailureWithConfig
        $ gitCheckoutExisting
        $ unpack
        $ pullRequestLocalHeadRef pullRequest

    lift $ mapAppError ConfigurationError loadConfig

cloneFailureWithConfig
    :: (HasLogFunc env, HasSystem env, HasDownloadFile env)
    => AppError
    -> RIO env CloneFailure
cloneFailureWithConfig err =
    CloneFailure err . Just <$> mapAppError ConfigurationError loadConfig

-- | Handle a Clone Failure with or without fallback Configuration
--
-- If the clone fails because the source PR has closed (and so the head branch
-- is gone), we should still attempt to our usual on-closure cleanup, but with
-- the default branch's configuration (needed to know the Restyled branch name).
--
-- In any other case, just re-throw the error as a @'PullRequestCloneError'@.
--
handleCloneFailure
    :: (HasLogFunc env, HasGitHub env, HasExit env)
    => PullRequest
    -> CloneFailure
    -> RIO env a
handleCloneFailure pullRequest = \case
    CloneFailure _ (Just config) | pullRequestIsClosed pullRequest -> do
        logWarn
            $ "Clone failure for closed PR,"
            <> " attempting cleanup using default branch configuration"
        (mRestyledPullRequest, restyledRef) <- findRestyled pullRequest config
        closeRestyledPullRequest' pullRequest mRestyledPullRequest restyledRef
        exitWithInfo "Source Pull Request is closed"

    CloneFailure err _ -> throwM $ toPullRequestCloneError err

toPullRequestFetchError :: AppError -> AppError
toPullRequestFetchError (GitHubError _ e) = PullRequestFetchError e
toPullRequestFetchError e = e

toPullRequestCloneError :: AppError -> AppError
toPullRequestCloneError (SystemError e) = PullRequestCloneError e
toPullRequestCloneError e = e
