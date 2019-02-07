module Restyler.Git
    ( gitClone
    , gitCheckout
    , gitCheckoutExisting
    , gitFetch
    , gitPush
    , gitPushForce
    , gitPushDelete
    , gitMergeBase
    , gitDiffNameOnly
    , gitCommitAll
    )
where

import Restyler.Prelude

import Restyler.App

gitClone :: MonadApp m => String -> FilePath -> m ()
gitClone url dir = callProcess "git" ["clone", "--quiet", url, dir]

gitCheckout :: MonadApp m => String -> m ()
gitCheckout branch = callProcess "git" ["checkout", "--quiet", "-b", branch]

gitCheckoutExisting :: MonadApp m => String -> m ()
gitCheckoutExisting branch = callProcess "git" ["checkout", "--quiet", branch]

gitFetch :: MonadApp m => String -> String -> m ()
gitFetch remoteRef localRef =
    callProcess "git" ["fetch", "origin", remoteRef <> ":" <> localRef]

gitPush :: MonadApp m => String -> m ()
gitPush branch = callProcess "git" ["push", "origin", branch]

gitPushForce :: MonadApp m => String -> m ()
gitPushForce branch =
    callProcess "git" ["push", "--force-with-lease", "origin", branch]

gitPushDelete :: MonadApp m => String -> m ()
gitPushDelete branch = callProcess "git" ["push", "origin", "--delete", branch]

gitMergeBase :: MonadApp m => String -> m (Maybe String)
gitMergeBase branch = do
    output <- readProcess "git" ["merge-base", branch, "HEAD"] ""
    pure $ listToMaybe $ lines output

gitDiffNameOnly :: MonadApp m => Maybe String -> m [FilePath]
gitDiffNameOnly mRef = do
    let args = ["diff", "--name-only"] <> maybeToList mRef
    lines <$> readProcess "git" args ""

gitCommitAll :: MonadApp m => String -> m String
gitCommitAll msg = do
    callProcess "git" ["commit", "-a", "--message", msg]

    -- TODO: gross
    unpack . chomp . pack <$> readProcess "git" ["rev-parse", "HEAD"] ""
