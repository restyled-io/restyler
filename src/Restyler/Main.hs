{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Main
    ( restylerMain
    ) where

import Restyler.Prelude

import GitHub.Client
import GitHub.Data
import Restyler.App
import Restyler.Clone
import Restyler.Comments
import Restyler.Config
import qualified Restyler.Content as Content
import Restyler.Options
import Restyler.PullRequest
import System.Exit (exitSuccess)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Process (callProcess)

restylerMain :: IO ()
restylerMain = do
    Options {..} <- parseOptions

    app <- loadApp =<< runGitHubThrow
        oAccessToken
        (getPullRequest oOwner oRepo oPullRequest)

    withinClonedRepo (remoteURL oAccessToken oOwner oRepo) $ runApp app $ do
        checkoutPullRequest

        unlessM runRestyler $ do
            logInfoN "No style differences found"
            me <- runGitHubThrow oAccessToken userInfoCurrent
            logInfoN $ "Clearing comments left by " <> tshow (userName me)
            pullRequest <- asks appConfig
            runGitHubThrow oAccessToken $ clearRestyledComments me pullRequest
            liftIO exitSuccess

        whenM tryAutoPush $ do
            logInfoN "Pushed to original PR"
            liftIO exitSuccess

        whenM tryUpdateBranch $ do
            logInfoN "Updated existing branch, skipping PR & comment"
            liftIO exitSuccess

        originalPr <- asks appConfig

        let
            createPr = restyledCreatePullRequest originalPr
            commentBody = restyledCommentBody originalPr

        restyledPr <- runGitHubThrow oAccessToken $ do
            pr <- createPullRequest oOwner oRepo createPr
            pr <$ leaveRestyledComment originalPr (commentBody pr)

        logInfoN $ "Opened Restyled PR "
            <> toPathPart oOwner <> "/"
            <> toPathPart oRepo <> "#"
            <> tshow (pullRequestNumber restyledPr)

restyledCommentBody :: PullRequest -> PullRequest -> Text
restyledCommentBody originalPr =
    if pullRequestIsFork originalPr
        then Content.commentBodyFork
        else Content.commentBody

checkoutPullRequest :: AppM PullRequest ()
checkoutPullRequest = do
    pullRequest@PullRequest {..} <- asks appConfig
    logInfoN $ "Checking out PR: " <> tshow pullRequest

    localRef <- if pullRequestIsFork pullRequest
        then do
            logInfoN "Fetching virtual ref for forked PR"
            fetchOrigin
                ("pull/" <> toPathPart pullRequestId <> "/head")
                (pullRequestLocalHeadRef pullRequest)
        else do
            logDebugN "Checking out local PR head"
            pure $ pullRequestHeadRef pullRequest

    checkoutBranch False localRef

runRestyler :: AppM PullRequest Bool
runRestyler = do
    callProcess "restyler-core"
        =<< filterM doesFileExist
        =<< changedPaths
        =<< asks (pullRequestBaseRef . appConfig)

    hBranch <- asks $ pullRequestLocalHeadRef . appConfig
    not . null <$> changedPaths hBranch

tryAutoPush :: AppM PullRequest Bool
tryAutoPush = do
    isAuto <- either (const False) cAuto <$> liftIO loadConfig
    pullRequest <- asks appConfig

    if isAuto && not (pullRequestIsFork pullRequest)
        then do
            commitAll Content.commitMessage
            pushOrigin $ pullRequestHeadRef pullRequest
            pure True
        else pure False

tryUpdateBranch :: AppM PullRequest Bool
tryUpdateBranch = do
    rBranch <- asks $ pullRequestRestyledRef . appConfig
    checkoutBranch True rBranch
    commitAll Content.commitMessage
    mBranchMessage <- branchHeadMessage ("origin/" <> rBranch)

    if mBranchMessage == Just Content.commitMessage
        then True <$ forcePushOrigin rBranch
        else False <$ pushOrigin rBranch

remoteURL :: Text -> Name Owner -> Name Repo -> Text
remoteURL token owner repo =
    "https://x-access-token:"
        <> token
        <> "@github.com/"
        <> untagName owner
        <> "/"
        <> untagName repo
        <> ".git"
