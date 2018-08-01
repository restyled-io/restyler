
module Restyler.Capabilities.Git
    ( MonadGit(..)
    )
where

import Restyler.Prelude

class MonadGit m where
    -- | @git clone \<url> \<directory>@
    cloneRepository :: Text -> FilePath -> m ()

    -- | @git checkout [-b] \<branch>@
    checkoutBranch :: Bool -> Text -> m ()

    -- | @git diff --name-only $(git merge-base \<branch>) HEAD@
    changedPaths :: Text -> m [FilePath]

    -- | @git commit --all --message \<message>@
    commitAll :: Text -> m ()

    -- | @git fetch origin \<remote>:\<local>@
    fetchOrigin :: Text -> Text -> m ()

    -- | @git push origin \<branch>@
    pushOrigin :: Text -> m ()

    -- | @git push --force-with-lease origin \<branch>@
    forcePushOrigin :: Text -> m ()
