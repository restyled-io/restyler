module Restyler.Clone
    ( clonePullRequest
    , checkoutPullRequest
    )
where

import Restyler.Prelude

import Restyler.Capabilities.Git
import Restyler.Capabilities.Logger
import Restyler.Capabilities.System
import Restyler.Options
import Restyler.PullRequest
import Restyler.WorkingDirectory

clonePullRequest
    :: ( MonadLogger m
       , MonadSystem m
       , MonadGit m
       , MonadReader env m
       , HasOptions env
       , HasWorkingDirectory env
       , HasPullRequest env
       )
    => m ()
clonePullRequest = do
    dir <- view workingDirectoryL
    token <- oAccessToken <$> view optionsL
    pullRequest <- view pullRequestL

    let cloneUrl = pullRequestCloneUrlToken token pullRequest
    gitClone (unpack cloneUrl) dir
    setCurrentDirectory dir

checkoutPullRequest
    :: (MonadLogger m, MonadGit m, MonadReader env m, HasPullRequest env)
    => m ()
checkoutPullRequest = do
    pullRequest <- view pullRequestL
    when (pullRequestIsNonDefaultBranch pullRequest) $ do
        logInfo "Non-default base branch"
        gitFetch
            (unpack $ pullRequestBaseRef pullRequest)
            (unpack $ pullRequestBaseRef pullRequest)

    when (pullRequestIsFork pullRequest) $ do
        logInfo "Fork PR"
        gitFetch
            (unpack $ pullRequestRemoteHeadRef pullRequest)
            (unpack $ pullRequestLocalHeadRef pullRequest)

    let localRef = pullRequestLocalHeadRef pullRequest
    gitCheckoutExisting $ unpack localRef
