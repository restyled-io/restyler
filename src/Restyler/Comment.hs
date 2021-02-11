module Restyler.Comment
    ( leaveRestyledComment
    , clearRestyledComments
    )
where

import Restyler.Prelude

import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub.Data (IssueComment(..), IssueNumber, SimpleUser(..))
import Restyler.Capabilities.GitHub
import qualified Restyler.Content as Content
import Restyler.PullRequest

-- | Leave a comment on the original PR, mentioning the given Restyled PR
leaveRestyledComment
    :: MonadGitHub m
    => PullRequest -- ^ Original PR
    -> IssueNumber -- ^ Restyled PR Number
    -> m ()
leaveRestyledComment pullRequest =
    createPullRequestComment pullRequest . Content.commentBody

-- | Locate any comments left by us on the origin PR and delete them
clearRestyledComments :: MonadGitHub m => PullRequest -> m ()
clearRestyledComments pullRequest = do
    comments <- getPullRequestComments pullRequest
    for_ (V.filter isRestyledComment comments) $ \comment -> do
        deletePullRequestComment pullRequest comment

commentUserName :: IssueComment -> Text
commentUserName = untagName . simpleUserLogin . issueCommentUser

isRestyledComment :: IssueComment -> Bool
isRestyledComment = isRestyledBotUserName . commentUserName

isRestyledBotUserName :: Text -> Bool
isRestyledBotUserName =
    (&&) <$> ("restyled-io" `T.isPrefixOf`) <*> ("[bot]" `T.isSuffixOf`)
