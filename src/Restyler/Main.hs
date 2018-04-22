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
import GitHub.Data
import Restyler.Clone
import Restyler.Options
import Restyler.PullRequest
import System.Directory (doesFileExist)
import System.Exit (die, exitSuccess)
import System.Process (callProcess)
import Text.Shakespeare.Text (st)

restylerMain :: IO ()
restylerMain = handleIO (die . show) $ do
    Options{..} <- parseOptions

    pullRequest <- runGitHubThrow oAccessToken (getPullRequest oOwner oRepo oPullRequest)

    let isFork = pullRequestIsFork pullRequest
        prNumber = pullRequestNumber pullRequest
        bBranch = pullRequestCommitRef $ pullRequestBase pullRequest
        hBranch =
            if isFork
                then "pr/" <> tshow prNumber -- fetch virtual ref as this
                else pullRequestCommitRef $ pullRequestHead pullRequest
        rBranch = hBranch <> "-restyled"
        rTitle = pullRequestTitle pullRequest <> " (Restyled)"
        commitMessage = "Restyled"

    withinClonedRepo (remoteURL oAccessToken oOwner oRepo) $ do
        when isFork $ fetchOrigin $ "pull/" <> tshow prNumber <> "/head:" <> hBranch
        checkoutBranch False hBranch

        paths <- filterM doesFileExist =<< changedPaths bBranch
        callProcess "restyler-core" paths

        wasRestyled <- not . null <$> changedPaths hBranch
        unless wasRestyled $ do
            putStrLn "No style differences found"
            exitSuccess

        checkoutBranch True rBranch
        commitAll commitMessage

        -- If a "-restyled" branch exists with a "Restyled" commit, this is a
        -- synchronize event and we should just update our already-existing PR
        -- with our fresh restyled commit.
        branchExists <- (== Just commitMessage)
            <$> branchHeadMessage ("origin/" <> rBranch)

        when branchExists $ do
            putStrLn "Restyled branch exists. Force pushing and skipping PR & comment"
            forcePushOrigin rBranch
            exitSuccess

        -- Normal path
        pushOrigin rBranch

    runGitHubThrow oAccessToken $ do
        pr <- createPullRequest oOwner oRepo CreatePullRequest
            { createPullRequestTitle = rTitle
            , createPullRequestBody = ""
            , createPullRequestHead = rBranch
            , createPullRequestBase =
                -- We can't open a PR in their fork, so we open a PR in our own
                -- repository against the base branch. It'll have their changes
                -- and our restyling as separate commits.
                if isFork
                    then bBranch
                    else hBranch
            }

        void
            $ createComment oOwner oRepo (asIssueId oPullRequest)
            $ restyledCommentBody pr

restyledCommentBody :: PullRequest -> Text
restyledCommentBody pullRequest = [st|
Hi there!

I just wanted to let you know that some code in this PR might not match the
team's preferred styles. This process isn't perfect, but when we ran some
auto-reformatting tools on it there were differences. Those differences can be
seen in ##{pullRequestNumber pullRequest}.

#{restyledAction pullRequest}

Thanks,
[Restyled.io][]

[restyled.io]: https://restyled.io
[documentation]: https://restyled.io/docs#disabling
|]

restyledAction :: PullRequest -> Text
restyledAction pullRequest
    | pullRequestIsFork pullRequest = [st|
Your PR was opened from a fork, so we're unable to open our PR with yours as the
base branch. Therefore, the PR linked above was opened directly against
`#{bBranch}`. It includes your changes and another commit to adjust styling.

If you're interested in incorporating the style changes in your PR, you can do
that locally with something like:

```console
git remote add upstream #{pullRequestRepoURL pullRequest}
git fetch upstream pull/#{pullRequestNumber pullRequest}/head
git merge --ff-only FETCH_HEAD
git push
```
|]
    | otherwise = [st|
To incorporate the changes, merge that PR into yours.

Sorry if this was unexpected. To disable it, see our [documentation].
|]
  where
    bBranch = pullRequestCommitRef $ pullRequestBase pullRequest

remoteURL :: Text -> Name Owner -> Name Repo -> Text
remoteURL token owner repo = "https://x-access-token:"
    <> token <> "@github.com/"
    <> untagName owner <> "/"
    <> untagName repo <> ".git"

asIssueId :: Id PullRequest -> Id Issue
asIssueId = mkId Proxy . untagId
