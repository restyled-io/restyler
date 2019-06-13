module Restyler.Main
    ( restylerMain
    ) where

import Restyler.Prelude

import Restyler.App.Class
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
    pullRequest <- view pullRequestL

    whenConfigNonEmpty cRemoteFiles $ traverse_ downloadRemoteFile

    unlessM isAutoPush $ gitCheckout $ unpack $ pullRequestRestyledRef
        pullRequest

    results <- restyle
    logDebug $ "Restyling results: " <> displayShow results

    unless (any restylerCommittedChanges results) $ do
        clearRestyledComments
        closeRestyledPullRequest
        sendPullRequestStatus NoDifferencesStatus
        exitWithInfo "No style differences found"

    whenM isAutoPush $ do
        gitPush $ unpack $ pullRequestHeadRef pullRequest
        exitWithInfo "Pushed to original PR"

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
