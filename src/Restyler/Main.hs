module Restyler.Main
    ( restylerMain
    ) where

import Restyler.Prelude

import Restyler.App.Class
import Restyler.App.Error
import Restyler.Comment
import Restyler.Config
import Restyler.Git
import Restyler.PullRequest
import Restyler.PullRequest.Restyled
import Restyler.PullRequest.Status
import Restyler.RemoteFile
import Restyler.Restyler.Run
import Restyler.RestylerResult

restylerMain
    :: ( HasLogFunc env
       , HasConfig env
       , HasPullRequest env
       , HasRestyledPullRequest env
       , HasSystem env
       , HasExit env
       , HasProcess env
       , HasDownloadFile env
       , HasGitHub env
       )
    => RIO env a
restylerMain = do
    whenConfigNonEmpty cRemoteFiles $ traverse_ downloadRemoteFile

    results <- restyle
    logDebug $ "Restyling results: " <> displayShow results

    unless (any restylerCommittedChanges results) $ do
        clearRestyledComments
        closeRestyledPullRequest
        sendPullRequestStatus NoDifferencesStatus
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

    mRestyledPullRequest <- view restyledPullRequestL
    restyledUrl <- case mRestyledPullRequest of
        Just restyledPr -> do
            updateRestyledPullRequest
            pure $ simplePullRequestHtmlUrl restyledPr
        Nothing -> do
            restyledPr <- createRestyledPullRequest results
            whenConfig cCommentsEnabled $ leaveRestyledComment restyledPr
            pure $ pullRequestHtmlUrl restyledPr

    sendPullRequestStatus $ DifferencesStatus restyledUrl
    exitWithInfo "Restyling successful"

downloadRemoteFile
    :: (HasLogFunc env, HasDownloadFile env) => RemoteFile -> RIO env ()
downloadRemoteFile RemoteFile {..} = do
    logInfo $ fromString $ "Fetching remote file: " <> rfPath
    downloadFile (getUrl rfUrl) rfPath

restyle
    :: ( HasLogFunc env
       , HasConfig env
       , HasPullRequest env
       , HasSystem env
       , HasProcess env
       )
    => RIO env [RestylerResult]
restyle = do
    restylers <- cRestylers <$> view configL
    pullRequest <- view pullRequestL
    pullRequestPaths <- changedPaths $ pullRequestBaseRef pullRequest
    runRestylers restylers pullRequestPaths

changedPaths :: HasProcess env => Text -> RIO env [FilePath]
changedPaths branch = do
    ref <- maybe branch pack <$> gitMergeBase (unpack branch)
    gitDiffNameOnly $ Just $ unpack ref

isAutoPush :: (HasConfig env, HasPullRequest env) => RIO env Bool
isAutoPush = do
    isAuto <- cAuto <$> view configL
    pullRequest <- view pullRequestL
    pure $ isAuto && not (pullRequestIsFork pullRequest)
