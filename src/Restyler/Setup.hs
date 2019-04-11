module Restyler.Setup
    ( restylerSetup
    ) where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import GitHub.Endpoints.PullRequests hiding (pullRequest)
import Restyler.App.Class
import Restyler.App.Error
import Restyler.Config
import Restyler.Git
import Restyler.Options
import Restyler.PullRequest

restylerSetup
    :: ( HasCallStack
       , HasOptions env
       , HasWorkingDirectory env
       , HasSystem env
       , HasProcess env
       , HasGitHub env
       )
    => RIO env (PullRequest, Maybe SimplePullRequest, Config)
restylerSetup = do
    Options {..} <- view optionsL

    pullRequest <-
        mapAppError toPullRequestFetchError $ runGitHub $ pullRequestR
            oOwner
            oRepo
            oPullRequest

    mRestyledPullRequest <-
        (`catchAny` const (pure Nothing))
        $ runGitHubFirst
        $ pullRequestsForR oOwner oRepo
        $ pullRequestRestyledMod pullRequest

    setupClone pullRequest

    configExists <- doesFileExist configPath
    config <- if configExists
        then decodeConfig =<< readFile configPath
        else pure defaultConfig

    pure (pullRequest, mRestyledPullRequest, config)

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

decodeConfig :: MonadUnliftIO m => Text -> m Config
decodeConfig =
    either (throwIO . ConfigurationError) pure . Yaml.decodeEither' . encodeUtf8

toPullRequestFetchError :: AppError -> AppError
toPullRequestFetchError (GitHubError e) = PullRequestFetchError e
toPullRequestFetchError e = e

toPullRequestCloneError :: AppError -> AppError
toPullRequestCloneError (SystemError e) = PullRequestCloneError e
toPullRequestCloneError e = e
