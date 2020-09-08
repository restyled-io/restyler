{-# LANGUAGE LambdaCase #-}

module Restyler.RestylerResult
    ( RestylerResult(..)
    , noPathsRestylerResult
    , getRestylerResult
    , restylerCommittedChanges
    )
where

import Restyler.Prelude

import Restyler.CommitTemplate
import Restyler.Config
import Restyler.Git
import Restyler.Restyler

data RestyleOutcome
    = NoPaths
    | NoChanges
    | ChangesCommitted [FilePath] Text

instance Display RestyleOutcome where
    display = \case
        NoPaths -> "no paths to restyle"
        NoChanges -> "nothing restyled"
        ChangesCommitted paths sha ->
            "updated "
                <> displayShow (length paths)
                <> " file(s),"
                <> " commited in "
                <> display sha

data RestylerResult = RestylerResult
    { rrRestyler :: Restyler
    , rrOutcome :: RestyleOutcome
    }

instance Display RestylerResult where
    display RestylerResult {..} =
        fromString (rName rrRestyler) <> ": " <> display rrOutcome

-- | A @'RestylerResult'@ indicating there were no paths to restyle
noPathsRestylerResult :: Restyler -> RestylerResult
noPathsRestylerResult r = RestylerResult r NoPaths

-- | Build a @'RestylerResult'@ based on what @git@ says
--
-- N.B. This will create commits if appropriate.
--
getRestylerResult
    :: (HasConfig env, HasGit env) => Restyler -> RIO env RestylerResult
getRestylerResult r = RestylerResult r <$> getRestyleOutcome r

-- | Does this @'RestylerResult'@ indicate changes were comitted?
restylerCommittedChanges :: RestylerResult -> Bool
restylerCommittedChanges = committedChanges . rrOutcome
  where
    committedChanges (ChangesCommitted _ _) = True
    committedChanges _ = False

getRestyleOutcome
    :: (HasConfig env, HasGit env) => Restyler -> RIO env RestyleOutcome
getRestyleOutcome restyler = do
    changedPaths <- gitDiffNameOnly Nothing

    if null changedPaths
        then pure NoChanges
        else do
            template <- cCommitTemplate <$> view configL
            let inputs = CommitTemplateInputs { ctiRestyler = restyler }
                commitMessage = renderCommitTemplate inputs template
            ChangesCommitted changedPaths . pack <$> gitCommitAll commitMessage
