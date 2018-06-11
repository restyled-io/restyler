module Restyler.Capabilities.GitHub
    ( MonadGitHub(..)
    )
where

import Restyler.Prelude

class MonadGitHub m where
    getPullRequest :: Name Owner -> Name Repo -> Id PullRequest -> m PullRequest
    findPullRequest :: Name Owner -> Name Repo -> Text -> Text -> m (Maybe SimplePullRequest)
    createPullRequest :: Name Owner -> Name Repo -> CreatePullRequest -> m PullRequest
    updatePullRequest :: Name Owner -> Name Repo -> Id PullRequest -> EditPullRequest -> m PullRequest
    getComments :: Name Owner -> Name Repo -> Id Issue -> m (Vector IssueComment)
    createComment :: Name Owner -> Name Repo -> Id Issue -> Text -> m ()
    deleteComment :: Name Owner -> Name Repo -> Id Comment -> m ()
    createStatus :: Name Owner -> Name Repo -> Name Commit -> NewStatus -> m ()
