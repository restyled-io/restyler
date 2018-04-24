{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Main
    ( restylerMain
    ) where

import Data.Proxy
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GitHub.Client
import GitHub.Data
import Restyler.App
import Restyler.Clone
import Restyler.Options
import Restyler.PullRequest
import System.Exit (exitSuccess)
import Text.Shakespeare.Text (st)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Process (callProcess)

restylerMain :: IO ()
restylerMain = do
    Options{..} <- parseOptions

    app <- loadApp =<< runGitHubThrow oAccessToken
        (getPullRequest oOwner oRepo oPullRequest)

    withinClonedRepo (remoteURL oAccessToken oOwner oRepo) $
        runApp app $ do
            checkoutPullRequest

            unlessM runRestyler $ do
                logInfoN "No style differences found"
                liftIO exitSuccess

            whenM tryUpdateBranch $ do
                logInfoN "Existing branch updated. Skipping PR & comment"
                liftIO exitSuccess

            createPr <- asks $ restyledCreatePullRequest . appConfig
            runGitHubThrow oAccessToken $ do
                pr <- createPullRequest oOwner oRepo createPr

                void
                    $ createComment oOwner oRepo (asIssueId oPullRequest)
                    $ restyledCommentBody pr

checkoutPullRequest :: AppM PullRequest ()
checkoutPullRequest = do
    pullRequest@PullRequest{..} <- asks appConfig
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

tryUpdateBranch :: AppM PullRequest Bool
tryUpdateBranch = do
    rBranch <- asks $ pullRequestRestyledRef . appConfig
    checkoutBranch True rBranch
    commitAll commitMessage
    mBranchMessage <- branchHeadMessage ("origin/" <> rBranch)

    if mBranchMessage == Just commitMessage
        then True <$ forcePushOrigin rBranch
        else False <$ pushOrigin rBranch
  where
    commitMessage = "Restyled"

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
    bBranch = pullRequestBaseRef pullRequest

remoteURL :: Text -> Name Owner -> Name Repo -> Text
remoteURL token owner repo = "https://x-access-token:"
    <> token <> "@github.com/"
    <> untagName owner <> "/"
    <> untagName repo <> ".git"

asIssueId :: Id PullRequest -> Id Issue
asIssueId = mkId Proxy . untagId

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = do
    result <- condition
    when result action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condition action = do
    result <- condition
    unless result action

tshow :: Show a => a -> Text
tshow = T.pack . show
