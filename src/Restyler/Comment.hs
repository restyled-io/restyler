module Restyler.Comment
    ( leaveRestyledComment
    , clearRestyledComments
    )
where

import Restyler.Prelude

import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub.Endpoints.Issues.Comments
import Restyler.App.Class
import Restyler.App.Error (warnIgnore)
import qualified Restyler.Content as Content
import Restyler.PullRequest

-- | Leave a comment on the original PR, mentioning the given Restyled PR
leaveRestyledComment
    :: (HasCallStack, HasGitHub env)
    => PullRequest -- ^ Original PR
    -> IssueNumber -- ^ Restyled PR Number
    -> RIO env ()
leaveRestyledComment pullRequest n = runGitHub_ $ createCommentR
    (pullRequestOwnerName pullRequest)
    (pullRequestRepoName pullRequest)
    (pullRequestNumber pullRequest)
    (Content.commentBody n)

-- | Locate any comments left by us on the origin PR and delete them
clearRestyledComments
    :: (HasCallStack, HasLogFunc env, HasGitHub env)
    => PullRequest
    -> RIO env ()
clearRestyledComments pullRequest = do
    comments <- runGitHub $ commentsR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestNumber pullRequest)
        FetchAll

    for_ (V.filter isRestyledComment comments) $ \comment -> do
        logInfo
            $ "Deleting comment "
            <> displayShow (issueCommentId comment)
            <> " by "
            <> displayShow (commentUserName comment)

        -- FIXME: I think deleteCommentR is broken. GitHub's Request fixes
        -- MtJSON, which is not MtUnit, and so we are expecting to parse a JSON
        -- response here, but GitHub is (rightfully) returning 204 No Content
        -- and failing to parse. I need to reproduce this minimally and report
        -- it, for now we just make comment-deletion best-effort.
        handleAny warnIgnore $ runGitHub_ $ deleteCommentR
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
