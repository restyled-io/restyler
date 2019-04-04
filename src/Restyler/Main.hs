{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Main
    ( restylerMain
    ) where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import Restyler.App
import Restyler.Comment
import Restyler.Config
import Restyler.Git
import Restyler.PullRequest
import Restyler.PullRequest.Restyled
import Restyler.PullRequest.Status
import Restyler.PullRequestSpec
import Restyler.RemoteFile
import Restyler.Restyler.Run
import Restyler.RestylerResult

restylerMain :: (HasCallStack, MonadApp m) => m ()
restylerMain = do
    whenM (asks $ pullRequestIsClosed . appPullRequest) $ do
        closeRestyledPullRequest
        exitWithInfo "Source Pull Request is closed"

    whenConfig (not . cEnabled . appConfig)
        $ exitWithInfo "Restyler disabled by config"
    logIntentions

    whenConfigNonEmpty (cRemoteFiles . appConfig) $ traverse_ downloadRemoteFile

    unlessM isAutoPush $ do
        branch <- asks $ pullRequestRestyledRef . appPullRequest
        gitCheckout $ unpack branch

    results <- restyle
    logDebugN $ "Restyling results: " <> tshow results

    unless (any restylerCommittedChanges results) $ do
        clearRestyledComments
        closeRestyledPullRequest
        sendPullRequestStatus NoDifferencesStatus
        exitWithInfo "No style differences found"

    whenM isAutoPush $ do
        branch <- asks $ pullRequestHeadRef . appPullRequest
        gitPush $ unpack branch
        exitWithInfo "Pushed to original PR"

    mRestyledPr <- asks appRestyledPullRequest
    restyledUrl <- case mRestyledPr of
        Just restyledPr -> do
            updateRestyledPullRequest
            pure $ simplePullRequestHtmlUrl restyledPr
        Nothing -> do
            restyledPr <- createRestyledPullRequest results
            whenConfig (cCommentsEnabled . appConfig)
                $ leaveRestyledComment restyledPr
            pure $ pullRequestHtmlUrl restyledPr

    sendPullRequestStatus $ DifferencesStatus restyledUrl
    logInfoN "Restyling successful"

logIntentions :: MonadApp m => m ()
logIntentions = do
    App {..} <- ask
    logInfoN $ "Restyling " <> showSpec (pullRequestSpec appPullRequest)
    logDebugN $ "Resolved configuration\n" <> decodeUtf8 (Yaml.encode appConfig)
    logRestyledPullRequest appPullRequest appRestyledPullRequest
  where
    logRestyledPullRequest _ Nothing = logInfoN "Restyled PR does not exist"
    logRestyledPullRequest pr (Just restyledPr) = do
        let
            -- Use the main PR's owner/repo because a SimplePullRequest doesn't
            -- have that information; it will always be the same.
            spec = showSpec (pullRequestSpec pr)
                { prsPullRequest = simplePullRequestNumber restyledPr
                }

        logInfoN $ "Restyled PR exists (" <> spec <> ")"

downloadRemoteFile :: MonadApp m => RemoteFile -> m ()
downloadRemoteFile RemoteFile {..} = do
    logInfoN $ "Fetching remote file: " <> pack rfPath
    downloadFile (getUrl rfUrl) rfPath

restyle :: MonadApp m => m [RestylerResult]
restyle = do
    restylers <- asks $ cRestylers . appConfig
    pullRequest <- asks appPullRequest
    pullRequestPaths <- changedPaths $ pullRequestBaseRef pullRequest
    runRestylers restylers pullRequestPaths

changedPaths :: MonadApp m => Text -> m [FilePath]
changedPaths branch = do
    ref <- maybe branch pack <$> gitMergeBase (unpack branch)
    gitDiffNameOnly $ Just $ unpack ref

isAutoPush :: MonadApp m => m Bool
isAutoPush = do
    isAuto <- asks $ cAuto . appConfig
    pullRequest <- asks appPullRequest
    pure $ isAuto && not (pullRequestIsFork pullRequest)

exitWithInfo :: MonadApp m => Text -> m ()
exitWithInfo msg = do
    logInfoN msg
    exitSuccess
