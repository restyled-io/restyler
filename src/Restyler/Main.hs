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
import Restyler.PullRequest.Status
import System.Exit (exitSuccess)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Process (callProcess)

restylerMain :: IO ()
restylerMain = do
    Options {..} <- parseOptions
    pullRequest <- runGitHubThrow oAccessToken
        $ getPullRequest oOwner oRepo oPullRequest

    run oAccessToken pullRequest

run :: Text -> PullRequest -> IO ()
run accessToken pullRequest = do
    app <- loadApp pullRequest

    let
        cloneUrl = remoteURL
            accessToken
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)

    withinClonedRepo cloneUrl $ runApp app $ do
        checkoutPullRequest

        unlessM runRestyler $ do
            logInfoN "No style differences found"
            runGitHubThrow accessToken $ do
                clearRestyledComments pullRequest
                void $ sendPullRequestStatus pullRequest NoDifferencesStatus
            liftIO exitSuccess

        whenM tryAutoPush $ do
            logInfoN "Pushed to original PR"
            liftIO exitSuccess

        whenM tryUpdateBranch $ do
            logInfoN "Updated existing branch, skipping PR & comment"
            liftIO exitSuccess

        originalPr <- asks appConfig
        restyledPr <- runGitHubThrow accessToken $ do
            pr <- createPullRequest
                (pullRequestOwnerName originalPr)
                (pullRequestRepoName originalPr)
                (restyledCreatePullRequest originalPr)

            pr <$ leaveRestyledComment
                originalPr
                (restyledCommentBody originalPr pr)

        logInfoN
            $ "Opened Restyled PR "
            <> toPathPart (pullRequestOwnerName restyledPr)
            <> "/"
            <> toPathPart (pullRequestRepoName restyledPr)
            <> "#"
            <> tshow (pullRequestNumber restyledPr)

restyledCommentBody :: PullRequest -> PullRequest -> Text
restyledCommentBody originalPr = if pullRequestIsFork originalPr
    then Content.commentBodyFork
    else Content.commentBody

checkoutPullRequest :: AppM PullRequest ()
checkoutPullRequest = do
    pullRequest@PullRequest {..} <- asks appConfig
    logInfoN $ "Checking out PR: " <> tshow pullRequest

    when (pullRequestIsFork pullRequest) $ do
        logInfoN "Fetching virtual ref for forked PR"
        fetchOrigin
            ("pull/" <> tshow pullRequestNumber <> "/head")
            (pullRequestLocalHeadRef pullRequest)

    checkoutBranch False $ pullRequestLocalHeadRef pullRequest

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
