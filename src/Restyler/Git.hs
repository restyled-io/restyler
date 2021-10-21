module Restyler.Git
    (
    -- * Class of actions that require the Clone
      HasGit(..)

    -- * Functions needed to establish a Clone
    -- | Therefore, they only require @'HasProcess'@
    , gitClone
    , gitFetch
    , gitCheckout
    , gitCheckoutExisting
    ) where

import Restyler.Prelude

import Restyler.App.Class

class HasGit env where
    gitPushForce :: String -> RIO env ()
    gitMergeBase :: String -> RIO env (Maybe String)
    gitDiffNameOnly :: Maybe String -> RIO env [FilePath]
    gitCommitAll :: String -> RIO env String

gitClone :: HasProcess env => String -> FilePath -> RIO env ()
gitClone url dir = callProcess "git" ["clone", "--quiet", url, dir]

gitFetch :: HasProcess env => String -> String -> RIO env ()
gitFetch remoteRef localRef =
    callProcess "git" ["fetch", "origin", remoteRef <> ":" <> localRef]

gitCheckout :: HasProcess env => String -> RIO env ()
gitCheckout branch =
    callProcess "git" ["checkout", "--no-progress", "-b", branch]

gitCheckoutExisting :: HasProcess env => String -> RIO env ()
gitCheckoutExisting branch =
    callProcess "git" ["checkout", "--no-progress", branch]
