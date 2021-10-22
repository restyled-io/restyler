module Restyler.Git
    (
    -- * Class of actions that require the Clone
      HasGit(..)

    -- * Functions needed to establish a Clone
    -- | Therefore, they only require @'HasProcess'@
    , gitInit
    , gitRemoteAdd
    , gitFetchDepth
    , gitCheckout
    , gitCheckoutExisting
    , gitCommitExists
    ) where

import Restyler.Prelude

import Restyler.App.Class

class HasGit env where
    gitPushForce :: String -> RIO env ()
    gitDiffNameOnly :: Maybe String -> RIO env [FilePath]
    gitCommitAll :: String -> RIO env String

gitInit :: HasProcess env => FilePath -> RIO env ()
gitInit dir = callProcess "git" ["init", "--quiet", dir]

gitRemoteAdd :: HasProcess env => String -> RIO env ()
gitRemoteAdd url = callProcess "git" ["remote", "add", "origin", url]

gitFetchDepth :: HasProcess env => Int -> String -> String -> RIO env ()
gitFetchDepth depth remoteRef localRef = callProcess
    "git"
    ["fetch", "origin", "--depth", show depth, remoteRef <> ":" <> localRef]

gitCheckout :: HasProcess env => String -> RIO env ()
gitCheckout branch =
    callProcess "git" ["checkout", "--no-progress", "-b", branch]

gitCheckoutExisting :: HasProcess env => String -> RIO env ()
gitCheckoutExisting branch =
    callProcess "git" ["checkout", "--no-progress", branch]

gitCommitExists :: HasProcess env => String -> RIO env Bool
gitCommitExists sha = do
    ec <- callProcessExitCode "git" ["cat-file", "-e", sha <> "^{commit}"]
    pure $ ec == ExitSuccess
