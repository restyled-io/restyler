{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Restyler.Model.Comment
    ( leaveRestyledComment
    , clearRestyledComments
    )
where

import Restyler.Prelude

import qualified Data.Text as T
import qualified Data.Vector as V
import Restyler.App
import Restyler.Capabilities.GitHub
import qualified Restyler.Content as Content
import Restyler.Model.PullRequest

-- | Leave a comment on the original PR, mentioning the given Restyled PR
leaveRestyledComment
    :: (MonadGitHub m, MonadReader App m) => PullRequest -> m ()
leaveRestyledComment restyledPr = do
    pullRequest <- asks appPullRequest

    let
        restyledCommentBody = if pullRequestIsFork pullRequest
            then Content.commentBodyFork
            else Content.commentBody

    createComment
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestIssueId pullRequest)
        (restyledCommentBody restyledPr)

-- | Locate any comments left by us on the origin PR and delete them
clearRestyledComments
    :: (MonadGitHub m, MonadLogger m, MonadReader App m) => m ()
clearRestyledComments = do
    pullRequest <- asks appPullRequest

    comments <- getComments
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestIssueId pullRequest)

    for_ (V.filter isRestyledComment comments) $ \comment -> do
        logDebugN
            $ "Deleting comment "
            <> tshow (issueCommentId comment)
            <> " by "
            <> commentUserName comment

        deleteComment
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
