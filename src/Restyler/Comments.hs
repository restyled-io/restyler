{-# LANGUAGE OverloadedStrings #-}

module Restyler.Comments
    ( leaveRestyledComment
    , clearRestyledComments
    ) where

import Restyler.Prelude

import qualified Data.Text as T
import GitHub.Client
import GitHub.Data
import Restyler.PullRequest

leaveRestyledComment :: PullRequest -> Text -> GitHubRW Comment
leaveRestyledComment pullRequest = createComment
    (pullRequestOwnerName pullRequest)
    (pullRequestRepoName pullRequest)
    (pullRequestIssueId pullRequest)

clearRestyledComments :: PullRequest -> GitHubRW ()
clearRestyledComments pullRequest = do
    comments <- getComments
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestIssueId pullRequest)

    for_ comments $ \comment -> when (isRestyledComment comment) $ deleteComment
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (mkId Proxy $ issueCommentId comment)

isRestyledComment :: IssueComment -> Bool
isRestyledComment =
    isRestyledBot . untagName . simpleUserLogin . issueCommentUser
  where
    isRestyledBot :: Text -> Bool
    isRestyledBot =
        (&&) <$> ("restyled-io" `T.isPrefixOf`) <*> ("[bot]" `T.isSuffixOf`)
