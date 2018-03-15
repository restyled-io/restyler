{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Restyler.Main
    ( restylerMain
    ) where

import ClassyPrelude

import Data.Proxy
import GitHub.Client
import GitHub.Endpoints.Installations
import Restyler.Clone
import Restyler.Config
import Restyler.Options
import Restyler.Run
import System.Exit (die, exitSuccess)
import Text.Shakespeare.Text (st)

restylerMain :: IO ()
restylerMain = handleIO (die . show) $ do
    Options{..} <- parseOptions
    AccessToken{..} <- createAccessToken oGitHubAppId oGitHubAppKey oInstallationId
    pullRequest <- runGitHubThrow atToken (getPullRequest oOwner oRepo oPullRequest)

    let bBranch = pullRequestCommitRef $ pullRequestBase pullRequest
        hBranch = pullRequestCommitRef $ pullRequestHead pullRequest
        rBranch = hBranch <> "-restyled"
        rTitle = pullRequestTitle pullRequest <> " (Restyled)"
        commitMessage = "Restyled"

    withinClonedRepo (remoteURL atToken oOwner oRepo) $ do
        checkoutBranch False hBranch

        config <- fromEitherM =<< loadConfig
        unless (cEnabled config) $ do
            putStrLn "Restyler disabled by config"
            exitSuccess

        paths <- changedPaths bBranch
        callRestylers config paths

        wasRestyled <- not . null <$> changedPaths hBranch
        unless wasRestyled $ do
            putStrLn "No style differences found"
            exitSuccess

        checkoutBranch True rBranch
        commitAll commitMessage

        -- If a "-restyled" branch exists with a "Restyled" commit, this is a
        -- synchronize event and we should just update our already-existing PR
        -- with our fresh restyled commit.
        branchExists <- (== Just commitMessage) <$> branchHeadMessage ("origin/" <> rBranch)

        when branchExists $ do
            putStrLn "Restyled branch exists. Force pushing and skipping PR & comment"
            forcePushOrigin rBranch
            exitSuccess

        -- Normal path
        pushOrigin rBranch

    runGitHubThrow atToken $ do
        pr <- createPullRequest oOwner oRepo CreatePullRequest
            { createPullRequestTitle = rTitle
            , createPullRequestBody = ""
            , createPullRequestHead = rBranch
            , createPullRequestBase = hBranch
            }

        void
            $ createComment oOwner oRepo (asIssueId oPullRequest)
            $ restyledCommentBody oRestyledRoot pr

restyledCommentBody :: Text -> PullRequest -> Text
restyledCommentBody root pullRequest = [st|
Hi there!

I just wanted to let you know that some code in this PR might not match the
team's preferred styles. This process isn't perfect, but when we ran some
auto-reformatting tools on it there were differences. Those differences can be
seen in ##{pullRequestNumber pullRequest}.

To incorporate the changes, merge that PR into yours.

Sorry if this was unexpected. To disable it, see our [documentation].

Thanks,
[Restyled.io][]

[restyled.io]: #{root}
[documentation]: #{root}/docs#disabling
|]

remoteURL :: Text -> Name Owner -> Name Repo -> Text
remoteURL token owner repo = "https://x-access-token:"
    <> token <> "@github.com/"
    <> untagName owner <> "/"
    <> untagName repo <> ".git"

asIssueId :: Id PullRequest -> Id Issue
asIssueId = mkId Proxy . untagId

fromEitherM :: MonadThrow m => Either String a -> m a
fromEitherM = either throwString pure
