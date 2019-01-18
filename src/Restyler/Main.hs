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
import Restyler.PullRequest
import Restyler.PullRequest.Restyled
import Restyler.PullRequest.Status
import Restyler.PullRequestSpec
import Restyler.RemoteFile
import Restyler.Restyler.Run

restylerMain :: (HasCallStack, MonadApp m) => m ()
restylerMain = do
    whenM (asks $ pullRequestIsClosed . appPullRequest) $ do
        closeRestyledPullRequest
        exitWithInfo "Source Pull Request is closed"

    unlessM configEnabled $ exitWithInfo "Restyler disabled by config"
    logIntentions

    remoteFiles <- asks $ cRemoteFiles . appConfig
    logInfoN $ "Fetching " <> tshow (length remoteFiles) <> " remote file(s)"
    for_ remoteFiles $ \RemoteFile {..} -> downloadFile (getUrl rfUrl) rfPath

    unlessM isAutoPush $ do
        branch <- asks $ pullRequestRestyledRef . appPullRequest
        callProcess "git" ["checkout", "-b", unpack branch]

    results <- restyle
    logDebugN $ "Restyling results: " <> tshow results

    unless (any restylerCommittedChanges results) $ do
        clearRestyledComments
        closeRestyledPullRequest
        sendPullRequestStatus NoDifferencesStatus
        exitWithInfo "No style differences found"

    whenM isAutoPush $ do
        branch <- asks $ pullRequestHeadRef . appPullRequest
        callProcess "git" ["push", "origin", unpack branch]
        exitWithInfo "Pushed to original PR"

    mRestyledPr <- asks appRestyledPullRequest
    restyledUrl <- case mRestyledPr of
        Just restyledPr -> do
            updateRestyledPullRequest
            pure $ simplePullRequestHtmlUrl restyledPr
        Nothing -> do
            restyledPr <- createRestyledPullRequest
            whenM commentsEnabled $ leaveRestyledComment restyledPr
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

configEnabled :: MonadApp m => m Bool
configEnabled = asks $ cEnabled . appConfig

commentsEnabled :: MonadApp m => m Bool
commentsEnabled = asks $ cCommentsEnabled . appConfig

restyle :: MonadApp m => m [RestylerResult]
restyle = do
    config <- asks appConfig
    pullRequest <- asks appPullRequest
    pullRequestPaths <- changedPaths $ pullRequestBaseRef pullRequest
    runRestylers (cRestylers config) pullRequestPaths

changedPaths :: MonadApp m => Text -> m [FilePath]
changedPaths branch = do
    output <- lines
        <$> readProcess "git" ["merge-base", unpack branch, "HEAD"] ""
    let ref = maybe branch pack $ listToMaybe output
    lines <$> readProcess "git" ["diff", "--name-only", unpack ref] ""

isAutoPush :: MonadApp m => m Bool
isAutoPush = do
    isAuto <- asks $ cAuto . appConfig
    pullRequest <- asks appPullRequest
    pure $ isAuto && not (pullRequestIsFork pullRequest)

exitWithInfo :: MonadApp m => Text -> m ()
exitWithInfo msg = do
    logInfoN msg
    exitSuccess
