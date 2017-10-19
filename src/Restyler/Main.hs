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
import Restyler.Options
import Restyler.Run
import Text.Shakespeare.Text (st)
import System.Exit (die)
import qualified Data.Text as T

restylerMain :: IO ()
restylerMain = do
    Options{..} <- parseOptions
    AccessToken{..} <- createAccessToken oGitHubAppId oGitHubAppKey oInstallationId
    pullRequest <- either (die . show) return
        =<< runGitHub atToken (getPullRequest oOwner oRepo oPullRequest)

    let bBranch = pullRequestCommitRef $ pullRequestBase pullRequest
        hBranch = pullRequestCommitRef $ pullRequestHead pullRequest
        rBranch = hBranch <> "-restyled"
        rTitle = pullRequestTitle pullRequest <> " (Restyled)"

    withinClonedRepo (remoteURL atToken oOwner oRepo) $ do
        checkoutBranch False hBranch
        either die return =<< callRestylers bBranch
        checkoutBranch True rBranch
        commitAll $ restyledCommitMessage oRestyledRoot pullRequest
        pushOrigin rBranch

    result <- runGitHub atToken $ do
        pr <- createPullRequest oOwner oRepo CreatePullRequest
            { createPullRequestTitle = rTitle
            , createPullRequestBody = ""
            , createPullRequestHead = hBranch
            , createPullRequestBase = rBranch
            }

        void
            $ createComment oOwner oRepo (asIssueId oPullRequest)
            $ restyledCommentBody oRestyledRoot pr

    either (die . show) return result

-- TODO: template with more details
restyledCommitMessage :: Text -> PullRequest -> Text
restyledCommitMessage _ _ = "Restyled"

restyledCommentBody :: Text -> PullRequest -> Text
restyledCommentBody root pullRequest = [st|
Hi there!

I just wanted to let you know that some code in this PR might not match the
team's preferred styles. This process isn't perfect, but when we ran some
auto-reformatting tools on it there were differences. Those differences can be
seen in ##{T.pack $ show $ pullRequestNumber pullRequest}.

To incorporate the changes, merge that PR into yours.

Sorry if this was unexpected. To disable it, see our [documentation].

Thanks,
[Restyled.io][]

[restyled.io]: #{root}
[documentation]: #{root}/docs#disable
|]

remoteURL :: Text -> Name Owner -> Name Repo -> Text
remoteURL token owner repo = "https://x-access-token:"
    <> token <> "@github.com/"
    <> untagName owner <> "/"
    <> untagName repo <> ".git"

asIssueId :: Id PullRequest -> Id Issue
asIssueId = mkId Proxy . untagId
