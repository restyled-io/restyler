module Restyler.Git
  ( MonadGit (..)

    -- * @DerivingVia@
  , ActualGit (..)
  , NullGit (..)
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import System.Process.Typed

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
    )

instance MonadIO m => MonadGit (ActualGit m) where
  gitPush branch = callGit ["push", "origin", branch]
  gitPushForce branch = callGit ["push", "--force", "origin", branch]
  gitDiffNameOnly mRef = readGitLines $ ["diff", "--name-only"] <> maybeToList mRef
  gitFormatPatch mRef = readGit $ ["format-patch", "--stdout"] <> maybeToList mRef
  gitCommitAll msg = do
    runProcess_ $ proc "git" ["commit", "-a", "--message", msg]
    readGitChomp ["rev-parse", "HEAD"]
  gitCheckout branch = callGit ["checkout", "--no-progress", "-b", branch]
  gitInit = callGit ["init", "--quiet", "."]
  gitRemoteAdd name url = callGit ["remote", "add", name, url]
  gitFetch name refspec = callGit ["fetch", "--quiet", "--depth", "1", name, refspec]
  gitSwitch branch = callGit ["checkout", "--no-progress", branch]

callGit :: MonadIO m => [String] -> m ()
callGit = runProcess_ . proc "git"

readGit :: MonadIO m => [String] -> m Text
readGit = fmap decodeUtf8 . readProcessStdout_ . proc "git"

readGitChomp :: MonadIO m => [String] -> m String
readGitChomp = fmap (unpack . T.dropWhileEnd isSpace) . readGit

readGitLines :: MonadIO m => [String] -> m [String]
readGitLines = fmap (map unpack . lines) . readGit

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
