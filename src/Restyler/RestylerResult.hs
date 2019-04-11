module Restyler.RestylerResult
    ( RestylerResult(..)
    , noPathsRestylerResult
    , getRestylerResult
    , restylerCommittedChanges
    )
where

import Restyler.Prelude

import Restyler.App
import Restyler.Git
import Restyler.Restyler

data RestyleOutcome
    = NoPaths
    | NoChanges
    | ChangesCommitted [FilePath] Text
    deriving Show

data RestylerResult = RestylerResult
    { rrRestyler :: Restyler
    , rrOutcome :: RestyleOutcome
    }

instance Show RestylerResult where
    show RestylerResult {..} = rName rrRestyler <> ": " <> show rrOutcome

-- | A @'RestylerResult'@ indicating there were no paths to restyle
noPathsRestylerResult :: Restyler -> RestylerResult
noPathsRestylerResult r = RestylerResult r NoPaths

-- | Build a @'RestylerResult'@ based on what @git@ says
--
-- N.B. This will create commits if appropriate.
--
getRestylerResult :: HasProcess env => Restyler -> RIO env RestylerResult
getRestylerResult r = RestylerResult r <$> getRestyleOutcome r

-- | Does this @'RestylerResult'@ indicate changes were comitted?
restylerCommittedChanges :: RestylerResult -> Bool
restylerCommittedChanges = committedChanges . rrOutcome
  where
    committedChanges (ChangesCommitted _ _) = True
    committedChanges _ = False

getRestyleOutcome :: HasProcess env => Restyler -> RIO env RestyleOutcome
getRestyleOutcome restyler = do
    changedPaths <- gitDiffNameOnly Nothing

    if null changedPaths
        then pure NoChanges
        else ChangesCommitted changedPaths . pack <$> gitCommitAll commitMessage
    where commitMessage = "Restyled by " <> rName restyler
