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

gitClone :: HasProcess env => String -> FilePath -> RIO env ()
gitClone url dir = callProcess "git" ["clone", "--quiet", url, dir]

gitCheckout :: HasProcess env => String -> RIO env ()
gitCheckout branch =
    callProcess "git" ["checkout", "--no-progress", "-b", branch]

gitCheckoutExisting :: HasProcess env => String -> RIO env ()
gitCheckoutExisting branch =
    callProcess "git" ["checkout", "--no-progress", branch]

gitFetch :: HasProcess env => String -> String -> RIO env ()
gitFetch remoteRef localRef =
    callProcess "git" ["fetch", "origin", remoteRef <> ":" <> localRef]

gitPush :: HasProcess env => String -> RIO env ()
gitPush branch = callProcess "git" ["push", "origin", branch]

gitPushForce :: HasProcess env => String -> RIO env ()
gitPushForce branch =
    callProcess "git" ["push", "--force-with-lease", "origin", branch]

gitPushDelete :: HasProcess env => String -> RIO env ()
gitPushDelete branch = callProcess "git" ["push", "origin", "--delete", branch]

gitMergeBase :: HasProcess env => String -> RIO env (Maybe String)
gitMergeBase branch = do
    output <- readProcess "git" ["merge-base", branch, "HEAD"] ""
    pure $ listToMaybe $ lines output

gitDiffNameOnly :: HasProcess env => Maybe String -> RIO env [FilePath]
gitDiffNameOnly mRef = do
    let args = ["diff", "--name-only"] <> maybeToList mRef
    lines <$> readProcess "git" args ""

gitCommitAll :: HasProcess env => String -> RIO env String
gitCommitAll msg = do
    callProcess "git" ["commit", "-a", "--message", msg]

    -- TODO: gross
    unpack . chomp . pack <$> readProcess "git" ["rev-parse", "HEAD"] ""
