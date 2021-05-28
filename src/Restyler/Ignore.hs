module Restyler.Ignore
    ( IgnoredReason(..)
    , getIgnoredReason

    -- * Pure implementation for tests
    , getIgnoredReason'
    )
where

import Restyler.Prelude

import GitHub.Data (IssueLabel, User)
import Restyler.App.Class
import Restyler.Config
import Restyler.Config.Glob
import Restyler.PullRequest

data IgnoredReason
    = IgnoredByAuthor
    | IgnoredByBranch
    | IgnoredByLabels
    deriving stock (Eq, Show)

getIgnoredReason
    :: HasGitHub env => Config -> PullRequest -> RIO env (Maybe IgnoredReason)
getIgnoredReason config pullRequest = do
    getIgnoredReason'
            config
            (pullRequestUserLogin pullRequest)
            (pullRequestBaseRef pullRequest)
        <$> getPullRequestLabelNames pullRequest

getIgnoredReason'
    :: Foldable t
    => Config
    -> Name User
    -> Text
    -> t (Name IssueLabel)
    -> Maybe IgnoredReason
getIgnoredReason' Config {..} author branch labels = asum
    [ IgnoredByAuthor <$ guard (cIgnoreAuthors `matchAny` [author])
    , IgnoredByBranch <$ guard (cIgnoreBranches `matchAny` [branch])
    , IgnoredByLabels <$ guard (cIgnoreLabels `matchAny` labels)
    ]
