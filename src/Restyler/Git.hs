{-# LANGUAGE OverloadedStrings #-}

module Restyler.Git
    ( cloneRepository
    , checkoutBranch
    , changedPaths
    , commitAll
    , fetchOrigin
    , pushOrigin
    , forcePushOrigin
    , branchHeadMessage
    )
where

import Restyler.Prelude

import qualified Data.Text as T
import qualified System.Process as Process

cloneRepository :: Text -> FilePath -> AppM ()
cloneRepository url dir = do
    -- N.B. Re-implements @'Restyler.Process.callProcss'@ to avoid logging the
    -- access-token present in the clone URL.
    logDebugN $ "callProcess: " <> tshow ["git", "clone", masked, dir]
    liftIOApp $ Process.callProcess "git" ["clone", unpack url, dir]
  where
    masked = T.unpack $ scheme <> "://<creds>" <> T.dropWhile (/= '@') rest
    (scheme, rest) = T.breakOn "://" url

checkoutBranch :: Bool -> Text -> AppM ()
checkoutBranch b branch =
    callProcess "git" $ ["checkout"] ++ [ "-b" | b ] ++ [unpack branch]

changedPaths :: Text -> AppM [FilePath]
changedPaths branch =
    lines <$> readProcess "git" ["diff", "--name-only", unpack branch] ""

commitAll :: Text -> AppM ()
commitAll msg = callProcess "git" ["commit", "-am", unpack msg]

fetchOrigin :: Text -> Text -> AppM ()
fetchOrigin remoteRef localRef =
    callProcess "git" ["fetch", "origin", unpack $ remoteRef <> ":" <> localRef]

pushOrigin :: Text -> AppM ()
pushOrigin branch = callProcess "git" ["push", "origin", unpack branch]

forcePushOrigin :: Text -> AppM ()
forcePushOrigin branch =
    callProcess "git" ["push", "--force-with-lease", "origin", unpack branch]

branchHeadMessage :: Text -> AppM (Maybe Text)
branchHeadMessage branch = strip . pack <$$> readProcessMay
    "git"
    ["log", "-n", "1", "--format=%B", unpack branch]
    ""
