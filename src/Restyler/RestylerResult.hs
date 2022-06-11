module Restyler.RestylerResult
    ( RestylerResult(..)
    , noPathsRestylerResult
    , getRestylerResult
    , restylerCommittedChanges
    ) where

import Restyler.Prelude

import Restyler.CommitTemplate
import Restyler.Config
import Restyler.Git
import Restyler.Restyler

data RestyleOutcome
    = NoPaths
    | NoChanges
    | ChangesCommitted [FilePath] Text

data RestylerResult = RestylerResult
    { rrRestyler :: Restyler
    , rrOutcome :: RestyleOutcome
    }

-- | A @'RestylerResult'@ indicating there were no paths to restyle
noPathsRestylerResult :: Restyler -> RestylerResult
noPathsRestylerResult r = RestylerResult r NoPaths

-- | Build a @'RestylerResult'@ based on what @git@ says
--
-- N.B. This will create commits if appropriate.
--
getRestylerResult
    :: (MonadGit m, MonadReader env m, HasConfig env)
    => Restyler
    -> m RestylerResult
getRestylerResult r = RestylerResult r <$> getRestyleOutcome r

-- | Does this @'RestylerResult'@ indicate changes were comitted?
restylerCommittedChanges :: RestylerResult -> Bool
restylerCommittedChanges = committedChanges . rrOutcome
  where
    committedChanges (ChangesCommitted _ _) = True
    committedChanges _ = False

getRestyleOutcome
    :: (MonadGit m, MonadReader env m, HasConfig env)
    => Restyler
    -> m RestyleOutcome
getRestyleOutcome restyler = do
    changedPaths <- gitDiffNameOnly Nothing

    if null changedPaths
        then pure NoChanges
        else do
            template <- cCommitTemplate <$> view configL
            let inputs = CommitTemplateInputs { ctiRestyler = restyler }
                commitMessage = renderCommitTemplate inputs template
            ChangesCommitted changedPaths . pack <$> gitCommitAll commitMessage
