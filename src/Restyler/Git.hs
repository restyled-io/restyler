-- | Class of actions that require the Clone
module Restyler.Git
  ( MonadGit (..)

    -- * DerivingVia
  , ActualGit (..)
  , NullGit (..)
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
  gitInit :: HasCallStack => m ()
  gitRemoteAdd :: HasCallStack => String -> String -> m ()
  gitFetch :: HasCallStack => String -> String -> m ()
  gitSwitch :: HasCallStack => String -> m ()

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
  gitPushForce branch = callProcess "git" ["push", "--force", "origin", branch]
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
      <$> readProcess "git" ["rev-parse", "HEAD"]
  gitCheckout branch = callProcess "git" ["checkout", "--no-progress", "-b", branch]
  gitInit = callProcess "git" ["init", "--quiet", "."]
  gitRemoteAdd name url = callProcess "git" ["remote", "add", name, url]
  gitFetch name refspec = callProcess "git" ["fetch", "--quiet", "--depth", "1", name, refspec]
  gitSwitch branch = callProcess "git" ["checkout", "--no-progress", branch]

newtype NullGit m a = NullGit
  { unwrap :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

instance Monad m => MonadGit (NullGit m) where
  gitPush _ = pure ()
  gitPushForce _ = pure ()
  gitDiffNameOnly _ = pure []
  gitFormatPatch _ = pure ""
  gitCommitAll _ = pure ""
  gitCheckout _ = pure ()
  gitInit = pure ()
  gitRemoteAdd _ _ = pure ()
  gitFetch _ _ = pure ()
  gitSwitch _ = pure ()
