module Restyler.Ignore
  ( IgnoredReason (..)
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

getIgnoredReason
  :: (MonadUnliftIO m, MonadLogger m, MonadGitHub m)
  => Config
  -> PullRequest
  -> m (Maybe IgnoredReason)
getIgnoredReason config pullRequest = do
  getIgnoredReason''
    config
    (pullRequestUserLogin pullRequest)
    (pullRequestBaseRef pullRequest)
    <$> getPullRequestLabelNames pullRequest

getIgnoredReason'
  :: (Functor t, Foldable t)
  => Config
  -> Text
  -- ^ Author
  -> Text
  -- ^ Base ref
  -> t Text
  -- ^ Label names
  -> Maybe IgnoredReason
getIgnoredReason' config author branch labels =
  getIgnoredReason''
    config
    (mkName (Proxy @User) author)
    branch
    (mkName (Proxy @IssueLabel) <$> labels)

getIgnoredReason''
  :: Foldable t
  => Config
  -> Name User
  -> Text
  -> t (Name IssueLabel)
  -> Maybe IgnoredReason
getIgnoredReason'' Config {..} author branch labels =
  asum
    [ IgnoredByAuthor author <$ guard (cIgnoreAuthors `matchAny` [author])
    , IgnoredByBranch branch <$ guard (cIgnoreBranches `matchAny` [branch])
    , IgnoredByLabels <$> cIgnoreLabels `matchFirst` labels
    ]
