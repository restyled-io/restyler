module Restyler.Ignore
  ( IgnoredReason (..)
  , getIgnoredReason
  ) where

import Restyler.Prelude

import Data.Aeson (ToJSON)
import GitHub.Data (IssueLabel, User)
import Restyler.Config
import Restyler.Config.Glob

data IgnoredReason
  = IgnoredByAuthor (Name User)
  | IgnoredByBranch Text
  | IgnoredByLabels (Name IssueLabel)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

getIgnoredReason
  :: (Functor t, Foldable t)
  => Config
  -> Text
  -- ^ Author
  -> Text
  -- ^ Base ref
  -> t Text
  -- ^ Label names
  -> Maybe IgnoredReason
getIgnoredReason config author branch labels =
  ghGetIgnoredReason config ghUser branch ghLabels
 where
  ghUser = mkName (Proxy @User) author
  ghLabels = mkName (Proxy @IssueLabel) <$> labels

-- | TODO: get out of using GitHub types here
ghGetIgnoredReason
  :: Foldable t
  => Config
  -> Name User
  -> Text
  -> t (Name IssueLabel)
  -> Maybe IgnoredReason
ghGetIgnoredReason Config {..} author branch labels =
  asum
    [ IgnoredByAuthor author <$ guard (cIgnoreAuthors `matchAny` [author])
    , IgnoredByBranch branch <$ guard (cIgnoreBranches `matchAny` [branch])
    , IgnoredByLabels <$> cIgnoreLabels `matchFirst` labels
    ]
