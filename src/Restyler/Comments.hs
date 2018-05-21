module Restyler.Comments
    ( leaveRestyledComment
    , clearRestyledComments
    ) where

import Restyler.Prelude

import GitHub.Client
import GitHub.Data
import Restyler.PullRequest

leaveRestyledComment :: PullRequest -> Text -> GitHubRW Comment
leaveRestyledComment pullRequest = createComment
    (pullRequestOwnerName pullRequest)
    (pullRequestRepoName pullRequest)
    (pullRequestIssueId pullRequest)

clearRestyledComments :: User -> PullRequest -> GitHubRW ()
clearRestyledComments me pullRequest = do
    comments <- getComments
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestIssueId pullRequest)

    for_ comments $ \comment ->
        when (simpleUserLogin (issueCommentUser comment) == userLogin me)
            $ deleteComment
                  (pullRequestOwnerName pullRequest)
                  (pullRequestRepoName pullRequest)
                  (mkId Proxy $ issueCommentId comment)
