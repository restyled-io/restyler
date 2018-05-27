{-# LANGUAGE OverloadedStrings #-}

module Restyler.Comments
    ( leaveRestyledComment
    , clearRestyledComments
    )
where

import Restyler.Prelude

import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import qualified Restyler.Content as Content
import Restyler.GitHub
import Restyler.PullRequest

leaveRestyledComment :: PullRequest -> AppM ()
leaveRestyledComment restyledPr = do
    pullRequest <- asks appPullRequest

    let
        restyledCommentBody = if pullRequestIsFork pullRequest
            then Content.commentBodyFork
            else Content.commentBody

    runGitHub_ $ createCommentR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestIssueId pullRequest)
        (restyledCommentBody restyledPr)

clearRestyledComments :: AppM ()
clearRestyledComments = do
    pullRequest <- asks appPullRequest

    comments <- runGitHub $ commentsR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestIssueId pullRequest)
        FetchAll

    for_ (V.filter isRestyledComment comments) $ \comment -> do
        logDebugN
            $ "Deleting comment "
            <> tshow (issueCommentId comment)
            <> " by "
            <> commentUserName comment

        runGitHub_ $ deleteCommentR
            (pullRequestOwnerName pullRequest)
            (pullRequestRepoName pullRequest)
            (mkId Proxy $ issueCommentId comment)

commentUserName :: IssueComment -> Text
commentUserName = untagName . simpleUserLogin . issueCommentUser

isRestyledComment :: IssueComment -> Bool
isRestyledComment = isRestyledBotUserName . commentUserName

isRestyledBotUserName :: Text -> Bool
isRestyledBotUserName =
    (&&) <$> ("restyled-io" `T.isPrefixOf`) <*> ("[bot]" `T.isSuffixOf`)
