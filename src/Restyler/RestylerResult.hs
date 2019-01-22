{-# LANGUAGE RecordWildCards #-}

module Restyler.RestylerResult
    ( RestylerResult(..)
    , noPathsRestylerResult
    , getRestylerResult
    , restylerCommittedChanges
    )
where

import Restyler.Prelude

import Restyler.App
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
getRestylerResult :: MonadApp m => Restyler -> m RestylerResult
getRestylerResult r = RestylerResult r <$> getRestyleOutcome r

-- | Does this @'RestylerResult'@ indicate changes were comitted?
restylerCommittedChanges :: RestylerResult -> Bool
restylerCommittedChanges = committedChanges . rrOutcome
  where
    committedChanges (ChangesCommitted _ _) = True
    committedChanges _ = False

getRestyleOutcome :: MonadApp m => Restyler -> m RestyleOutcome
getRestyleOutcome restyler = do
    changedPaths <- gitDiffNameOnly

    if null changedPaths
        then pure NoChanges
        else ChangesCommitted changedPaths <$> commitChanges
  where
    gitDiffNameOnly = lines <$> readProcess "git" ["diff", "--name-only"] ""
    gitCommitAll msg = callProcess "git" ["commit", "-a", "--message", msg]
    gitRevParseHead = readProcess "git" ["rev-parse", "HEAD"] ""

    commitChanges = do
        gitCommitAll $ "Restyled by " <> rName restyler
        chomp . pack <$> gitRevParseHead
