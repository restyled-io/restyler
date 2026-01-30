{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- Module      : Restyler.Monad.Git
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Monad.Git
  ( MonadGit (..)

    -- * @DerivingVia@
  , ActualGit (..)
  , NullGit (..)
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.AnnotatedException
import System.Process.Typed

class Monad m => MonadGit m where
  isGitRepository :: HasCallStack => m Bool
  gitDiffNameOnly :: HasCallStack => Maybe String -> m [FilePath]
  gitCommit :: HasCallStack => String -> NonEmpty FilePath -> m String
  gitResetHard :: HasCallStack => String -> m ()

-- | An instance that invokes the real @git@
newtype ActualGit m a = ActualGit
  { unwrap :: m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadLogger
    , MonadReader env
    , MonadUnliftIO
    )

instance
  (HasLogger env, MonadLogger m, MonadReader env m, MonadUnliftIO m)
  => MonadGit (ActualGit m)
  where
  isGitRepository = (== ExitSuccess) <$> runGitExitCode ["rev-parse"]
  gitDiffNameOnly mRef = readGitLines $ ["diff", "--name-only"] <> maybeToList mRef
  gitCommit msg paths = do
    runGit_ $ ["commit", "--message", msg, "--"] <> toList paths
    readGitChomp ["rev-parse", "HEAD"]
  gitResetHard ref = runGit_ ["reset", "--hard", ref]

runGit_
  :: ( HasCallStack
     , HasLogger env
     , MonadLogger m
     , MonadReader env m
     , MonadUnliftIO m
     )
  => [String]
  -> m ()
runGit_ args = checkpointCallStack $ do
  logProc "git" args
  runProcess_ $ proc "git" args

runGitExitCode
  :: ( HasCallStack
     , HasLogger env
     , MonadLogger m
     , MonadReader env m
     , MonadUnliftIO m
     )
  => [String]
  -> m ExitCode
runGitExitCode args = checkpointCallStack $ do
  logProc "git" args
  runProcess $ proc "git" args

readGit
  :: ( HasCallStack
     , HasLogger env
     , MonadLogger m
     , MonadReader env m
     , MonadUnliftIO m
     )
  => [String]
  -> m Text
readGit args = checkpointCallStack $ do
  logProc "git" args
  decodeUtf8 <$> readProcessStdout_ (proc "git" args)

readGitChomp
  :: ( HasCallStack
     , HasLogger env
     , MonadLogger m
     , MonadReader env m
     , MonadUnliftIO m
     )
  => [String]
  -> m String
readGitChomp = fmap (unpack . T.dropWhileEnd isSpace) . readGit

readGitLines
  :: ( HasCallStack
     , HasLogger env
     , MonadLogger m
     , MonadReader env m
     , MonadUnliftIO m
     )
  => [String]
  -> m [String]
readGitLines = fmap (map unpack . lines) . readGit

-- | An instance where all operations no-op or return empty strings
newtype NullGit m a = NullGit
  { unwrap :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

instance Monad m => MonadGit (NullGit m) where
  isGitRepository = pure False
  gitDiffNameOnly _ = pure []
  gitCommit _ _ = pure ""
  gitResetHard _ = pure ()
