-- | Class of actions that require the Clone
module Restyler.Git
  ( MonadGit (..)

    -- * DerivingVia
  , ActualGit (..)
  , gitCloneBranchByRef
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.App.Class

class Monad m => MonadGit m where
  gitPush :: HasCallStack => String -> m ()
  gitPushForce :: HasCallStack => String -> m ()
  gitDiffNameOnly :: HasCallStack => Maybe String -> m [FilePath]
  gitFormatPatch :: HasCallStack => Maybe String -> m Text
  gitCommitAll :: HasCallStack => String -> m String
  gitCheckout :: HasCallStack => String -> m ()

newtype ActualGit m a = ActualGit
  { unwrap :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadSystem
    , MonadProcess
    )

instance (MonadSystem m, MonadProcess m) => MonadGit (ActualGit m) where
  gitPush branch = callProcess "git" ["push", "origin", branch]
  gitPushForce branch =
    callProcess "git" ["push", "--force", "origin", branch]
  gitDiffNameOnly mRef = do
    let args = ["diff", "--name-only"] <> maybeToList mRef
    map unpack . lines . pack <$> readProcess "git" args
  gitFormatPatch mRef = do
    let args = ["format-patch", "--stdout"] <> maybeToList mRef
    pack <$> readProcess "git" args
  gitCommitAll msg = do
    callProcess "git" ["commit", "-a", "--message", msg]
    unpack
      . T.dropWhileEnd isSpace
      . pack
      <$> readProcess
        "git"
        ["rev-parse", "HEAD"]
  gitCheckout branch = do
    callProcess "git" ["checkout", "--no-progress", "-b", branch]

-- | Shallow-clone a specific branch and check it out, by virtual ref
--
-- GitHub's @pulls/N/head@ ref isn't real enough to work with @clone --branch@,
-- so we do the functionally-equivalent thing of @init@/@remote-add@/@fetch@.
gitCloneBranchByRef
  :: MonadProcess m
  => String
  -- ^ Remote ref
  -> String
  -- ^ Local branch name
  -> String
  -- ^ URL
  -> m ()
gitCloneBranchByRef ref branch url = do
  callGit "init" ["--quiet", "."]
  callGit "remote" ["add", "origin", url]
  callGit "fetch" ["--quiet", "--depth", "1", "origin", ref <> ":" <> branch]
  callGit "checkout" ["--no-progress", branch]

callGit :: MonadProcess m => String -> [String] -> m ()
callGit subcommand args = callProcess "git" $ subcommand : args
