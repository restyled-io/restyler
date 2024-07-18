module Restyler.RestylerResult
  ( RestylerResult (..)
  , RestyleOutcome (..)
  , noPathsRestylerResult
  , getRestylerResult
  , restylerCommittedChanges
  ) where

import Restyler.Prelude

import Data.Aeson (ToJSON)
import Restyler.CommitTemplate
import Restyler.Config
import Restyler.Git
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
  :: MonadGit m
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
  :: MonadGit m
  => Config
  -> Restyler
  -> m RestyleOutcome
getRestyleOutcome config restyler = do
  changedPaths <- gitDiffNameOnly Nothing

  if null changedPaths
    then pure NoChanges
    else do
      let
        inputs = CommitTemplateInputs {restyler}
        commitMessage = renderCommitTemplate inputs $ cCommitTemplate config
      ChangesCommitted changedPaths . pack <$> gitCommitAll commitMessage
