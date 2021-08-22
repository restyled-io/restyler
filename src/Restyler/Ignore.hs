module Restyler.Ignore
    ( IgnoredReason(..)
    , getIgnoredReason

    -- * Pure implementation for tests
    , getIgnoredReason'
    ) where

import Restyler.Prelude

import GitHub.Data (IssueLabel, User)
import Restyler.App.Class
import Restyler.Config
import Restyler.Config.Glob
import Restyler.PullRequest

data IgnoredReason
    = IgnoredByAuthor (Name User)
    | IgnoredByBranch Text
    | IgnoredByLabels (Name IssueLabel)
    deriving stock (Eq, Show)

instance Display IgnoredReason where
    display = \case
        IgnoredByAuthor author -> "author (" <> display author <> ")"
        IgnoredByBranch branch -> "author (" <> display branch <> ")"
        IgnoredByLabels label -> "author (" <> display label <> ")"

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
    [ IgnoredByAuthor author <$ guard (cIgnoreAuthors `matchAny` [author])
    , IgnoredByBranch branch <$ guard (cIgnoreBranches `matchAny` [branch])
    , IgnoredByLabels <$> cIgnoreLabels `matchFirst` labels
    ]
