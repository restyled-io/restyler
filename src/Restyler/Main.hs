module Restyler.Main
    ( restylerMain
    ) where

import Restyler.Prelude

import Restyler.App.Class
import Restyler.App.Error
import Restyler.Comment
import Restyler.Config
import Restyler.Git
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequest.Restyled
import Restyler.PullRequest.Status
import Restyler.RemoteFile
import Restyler.Restyler.Run
import Restyler.RestylerResult

restylerMain
    :: ( HasLogFunc env
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       , HasRestyledPullRequest env
       , HasSystem env
       , HasExit env
       , HasProcess env
       , HasGit env
       , HasDownloadFile env
       , HasGitHub env
       )
    => RIO env a
restylerMain = do
    jobUrl <- oJobUrl <$> view optionsL
    whenConfigNonEmpty cRemoteFiles $ traverse_ downloadRemoteFile

    results <- restyle
    logDebug $ "Restyling results: " <> displayShow results

    unless (any restylerCommittedChanges results) $ do
        clearRestyledComments
        closeRestyledPullRequest
        sendPullRequestStatus $ NoDifferencesStatus jobUrl
        exitWithInfo "No style differences found"

    whenM isAutoPush $ do
        logInfo "Pushing Restyle commits to original PR"
        pullRequest <- view pullRequestL
        gitCheckoutExisting $ unpack $ pullRequestLocalHeadRef pullRequest
        gitMerge $ unpack $ pullRequestRestyledRef pullRequest

        handleAny warnIgnore $ do
            -- This will fail if other changes came in while we were restyling,
            -- but it also means that we should be working on a Job for those
            -- changes already
            gitPush $ unpack $ pullRequestHeadRef pullRequest
            exitWithInfo "Pushed Restyle commits to original PR"

    -- NB there is the edge-case of switching this off mid-PR. A previously
    -- opened Restyle PR would stop updating at that point.
    whenConfig (not . cPullRequests) $ do
        sendPullRequestStatus $ DifferencesStatus jobUrl
        logInfo "Not creating (or updating) Restyle PR, disabled by config"
        exitWithInfo "Restyling successful"

    mRestyledPullRequest <- view restyledPullRequestL
    restyledUrl <- case mRestyledPullRequest of
        Just restyledPr -> do
            updateRestyledPullRequest
            pure $ simplePullRequestHtmlUrl restyledPr
        Nothing -> do
            restyledPr <- createRestyledPullRequest results
            pure $ pullRequestHtmlUrl restyledPr

    sendPullRequestStatus $ DifferencesStatus $ Just restyledUrl
    exitWithInfo "Restyling successful"

restyle
    :: ( HasLogFunc env
       , HasConfig env
       , HasPullRequest env
       , HasSystem env
       , HasProcess env
       , HasGit env
       )
    => RIO env [RestylerResult]
restyle = do
    restylers <- cRestylers <$> view configL
    pullRequest <- view pullRequestL
    pullRequestPaths <- changedPaths $ pullRequestBaseRef pullRequest
    runRestylers restylers pullRequestPaths

changedPaths :: HasGit env => Text -> RIO env [FilePath]
changedPaths branch = do
    ref <- maybe branch pack <$> gitMergeBase (unpack branch)
    gitDiffNameOnly $ Just $ unpack ref

isAutoPush :: (HasConfig env, HasPullRequest env) => RIO env Bool
isAutoPush = do
    isAuto <- cAuto <$> view configL
    pullRequest <- view pullRequestL
    pure $ isAuto && not (pullRequestIsFork pullRequest)
