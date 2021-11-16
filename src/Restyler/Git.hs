module Restyler.Git
    (
    -- * Class of actions that require the Clone
      HasGit(..)
    , gitCloneBranchByRef
    ) where

import Restyler.Prelude

import Restyler.App.Class

class HasGit env where
    gitPush :: String -> RIO env ()
    gitPushForce :: String -> RIO env ()
    gitDiffNameOnly :: Maybe String -> RIO env [FilePath]
    gitCommitAll :: String -> RIO env String
    gitCheckout :: String -> RIO env ()

-- | Shallow-clone a specific branch and check it out, by virtual ref
--
-- GitHub's @pulls/N/head@ ref isn't real enough to work with @clone --branch@,
-- so we do the functionally-equivalent thing of @init@/@remote-add@/@fetch@.
--
gitCloneBranchByRef
    :: (HasProcess env, HasSystem env)
    => String -- ^ Remote ref
    -> String -- ^ Local branch name
    -> String -- ^ URL
    -> FilePath -- ^ Directory
    -> RIO env ()
gitCloneBranchByRef ref branch url dir = do
    callGit "init" ["--quiet", dir]
    setCurrentDirectory dir
    callGit "remote" ["add", "origin", url]
    callGit "fetch" ["--quiet", "--depth", "1", "origin", ref <> ":" <> branch]
    callGit "checkout" ["--no-progress", branch]

callGit :: HasProcess env => String -> [String] -> RIO env ()
callGit subcommand args = callProcess "git" $ subcommand : args
