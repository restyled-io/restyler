module Restyler.Capabilities.GitHub
    ( MonadGitHub(..)
    )
where

import Restyler.Prelude

class MonadGitHub m where
    getPullRequest :: Name Owner -> Name Repo -> Id PullRequest -> m PullRequest
    createPullRequest :: Name Owner -> Name Repo -> CreatePullRequest -> m PullRequest
    getComments :: Name Owner -> Name Repo -> Id Issue -> m (Vector IssueComment)
    createComment :: Name Owner -> Name Repo -> Id Issue -> Text -> m ()
    deleteComment :: Name Owner -> Name Repo -> Id Comment -> m ()
    createStatus :: Name Owner -> Name Repo -> Name Commit -> NewStatus -> m ()
