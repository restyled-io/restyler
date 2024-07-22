module Restyler.RestylerResult
  ( RestylerResult (..)
  , RestyleOutcome (..)
  , noPathsRestylerResult
  , getRestylerResult
  , restylerCommittedChanges
  ) where

import Restyler.Prelude

import Restyler.Config
import Restyler.Config.CommitTemplate
import Restyler.Git
import Restyler.Options.NoCommit
import Restyler.Restyler

data RestyleOutcome
  = NoPaths
  | NoChanges
  | ChangesCommitted [FilePath] Text
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data RestylerResult = RestylerResult
  { restyler :: Restyler
  , outcome :: RestyleOutcome
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | A @'RestylerResult'@ indicating there were no paths to restyle
noPathsRestylerResult :: Restyler -> RestylerResult
noPathsRestylerResult r = RestylerResult r NoPaths

-- | Build a @'RestylerResult'@ based on what @git@ says
--
-- N.B. This will create commits if appropriate.
getRestylerResult
  :: (MonadGit m, MonadReader env m, HasNoCommitOption env)
  => Config
  -> Restyler
  -> m RestylerResult
getRestylerResult config r = RestylerResult r <$> getRestyleOutcome config r

-- | Does this @'RestylerResult'@ indicate changes were comitted?
restylerCommittedChanges :: RestylerResult -> Bool
restylerCommittedChanges rr = committedChanges rr.outcome
 where
  committedChanges (ChangesCommitted _ _) = True
  committedChanges _ = False

getRestyleOutcome
  :: (MonadGit m, MonadReader env m, HasNoCommitOption env)
  => Config
  -> Restyler
  -> m RestyleOutcome
getRestyleOutcome config restyler = do
  noCommit <- getNoCommit
  changedPaths <- gitDiffNameOnly Nothing

  if null changedPaths
    then pure NoChanges
    else do
      sha <-
        if noCommit
          then pure "<commit skipped>"
          else gitCommitAll commitMessage
      pure $ ChangesCommitted changedPaths $ pack sha
 where
  inputs = CommitTemplateInputs {restyler}
  commitMessage = renderCommitTemplate inputs $ cCommitTemplate config
