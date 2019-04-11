module Restyler.Comment
    ( leaveRestyledComment
    , clearRestyledComments
    )
where

import Restyler.Prelude

import qualified Data.Text as T
import qualified Data.Vector as V
import Restyler.App.Class
import qualified Restyler.Content as Content
import Restyler.PullRequest
import GitHub.Endpoints.Issues.Comments hiding (comment, comments)

-- | Leave a comment on the original PR, mentioning the given Restyled PR
leaveRestyledComment
    :: (HasCallStack, HasPullRequest env, HasGitHub env)
    => PullRequest
    -> RIO env ()
leaveRestyledComment restyledPr = do
    pullRequest <- view pullRequestL

    runGitHub_ $ createCommentR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestIssueId pullRequest)
        (Content.commentBody restyledPr)

-- | Locate any comments left by us on the origin PR and delete them
clearRestyledComments
    :: (HasCallStack, HasLogFunc env, HasPullRequest env, HasGitHub env)
    => RIO env ()
clearRestyledComments = do
    pullRequest <- view pullRequestL

    comments <- runGitHub $ commentsR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestIssueId pullRequest)
        FetchAll

    for_ (V.filter isRestyledComment comments) $ \comment -> do
        logDebug
            $ "Deleting comment "
            <> displayShow (issueCommentId comment)
            <> " by "
            <> displayShow (commentUserName comment)

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
