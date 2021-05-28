module Restyler.Ignore
    ( IgnoredReason(..)
    , getIgnoredReason

    -- * Exported for testing
    , ignoreByLabels
    )
where

import Restyler.Prelude

import GitHub.Data (IssueLabel)
import Restyler.App.Class
import Restyler.Config
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
    pure $ asum [ignoreByLabels config labels]

ignoreByLabels :: Config -> Vector (Name IssueLabel) -> Maybe IgnoredReason
ignoreByLabels config labels = do
    guard $ labels `intersects` cIgnoreLabels config
    pure IgnoredReason
        { irStatusReason = "ignore labels"
        , irExitMessageSuffix = "based on its labels"
        }
