module Restyler.Ignore
    ( IgnoredReason(..)
    , getIgnoredReason

    -- * Exported for testing
    , ignoreByLabels
    )
where

import Restyler.Prelude

import GitHub.Data (IssueLabel, User)
import Restyler.App.Class
import Restyler.Config
import Restyler.Config.Glob
import Restyler.PullRequest

data IgnoredReason = IgnoredReason
    { irStatusReason :: Text
    , irExitMessageSuffix :: Text
    }
    deriving stock (Eq, Show)

getIgnoredReason
    :: HasGitHub env => Config -> PullRequest -> RIO env (Maybe IgnoredReason)
getIgnoredReason config pullRequest = do
    labels <- getPullRequestLabelNames pullRequest
    pure $ asum
        [ ignoreByAuthor config $ pullRequestUserLogin pullRequest
        , ignoreByLabels config labels
        , ignoreByBranch config $ pullRequestBaseRef pullRequest
        ]

ignoreByAuthor :: Config -> Name User -> Maybe IgnoredReason
ignoreByAuthor Config {..} author = do
    guard $ matchAny cIgnoreAuthors [author]
    pure IgnoredReason
        { irStatusReason = "ignore author"
        , irExitMessageSuffix = "based on its author"
        }

ignoreByLabels
    :: Foldable t => Config -> t (Name IssueLabel) -> Maybe IgnoredReason
ignoreByLabels Config {..} labels = do
    guard $ matchAny cIgnoreLabels labels
    pure IgnoredReason
        { irStatusReason = "ignore labels"
        , irExitMessageSuffix = "based on its labels"
        }

ignoreByBranch :: Config -> Text -> Maybe IgnoredReason
ignoreByBranch Config {..} branch = do
    guard $ matchAny cIgnoreBranches [branch]
    pure IgnoredReason
        { irStatusReason = "ignore branches"
        , irExitMessageSuffix = "based on its base branch"
        }
