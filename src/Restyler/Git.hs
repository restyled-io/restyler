-- | Class of actions that require the Clone
module Restyler.Git
    ( MonadGit(..)
    , gitCloneBranchByRef
    ) where

import Restyler.Prelude

import Restyler.App.Class

class Monad m => MonadGit m where
    gitPush :: String -> m ()
    gitPushForce :: String -> m ()
    gitDiffNameOnly :: Maybe String -> m [FilePath]
    gitCommitAll :: String -> m String
    gitCheckout :: String -> m ()

-- | Shallow-clone a specific branch and check it out, by virtual ref
--
-- GitHub's @pulls/N/head@ ref isn't real enough to work with @clone --branch@,
-- so we do the functionally-equivalent thing of @init@/@remote-add@/@fetch@.
--
gitCloneBranchByRef
    :: (MonadSystem m, MonadProcess m)
    => String -- ^ Remote ref
    -> String -- ^ Local branch name
    -> String -- ^ URL
    -> FilePath -- ^ Directory
    -> m ()
gitCloneBranchByRef ref branch url dir = do
    callGit "init" ["--quiet", dir]
    setCurrentDirectory dir
    callGit "remote" ["add", "origin", url]
    callGit "fetch" ["--quiet", "--depth", "1", "origin", ref <> ":" <> branch]
    callGit "checkout" ["--no-progress", branch]

callGit :: MonadProcess m => String -> [String] -> m ()
callGit subcommand args = callProcess "git" $ subcommand : args
