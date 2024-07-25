{-# LANGUAGE UndecidableInstances #-}

module Restyler.Git
  ( MonadGit (..)

    -- * @DerivingVia@
  , ActualGit (..)
  , NullGit (..)
  ) where

import Restyler.Prelude

import Blammo.Logging.Logger (flushLogger)
import Data.Text qualified as T
import Restyler.AnnotatedException
import System.Process.Typed

class Monad m => MonadGit m where
  gitDiffNameOnly :: HasCallStack => Maybe String -> m [FilePath]
  gitCommitAll :: HasCallStack => String -> m String

-- | An instance that invokes the real @git@
newtype ActualGit m a = ActualGit
  { unwrap :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    , MonadReader env
    )

instance
  (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasLogger env)
  => MonadGit (ActualGit m)
  where
  gitDiffNameOnly mRef = readGitLines $ ["diff", "--name-only"] <> maybeToList mRef
  gitCommitAll msg = do
    runProcess_ $ proc "git" ["commit", "-a", "--message", msg]
    readGitChomp ["rev-parse", "HEAD"]

readGit
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasCallStack
     )
  => [String]
  -> m Text
readGit args = checkpointCallStack $ do
  logDebug $ ("exec git " <> unwords (map pack args)) :# []
  flushLogger
  decodeUtf8 <$> readProcessStdout_ (proc "git" args)

readGitChomp
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasCallStack
     )
  => [String]
  -> m String
readGitChomp = fmap (unpack . T.dropWhileEnd isSpace) . readGit

readGitLines
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasCallStack
     )
  => [String]
  -> m [String]
readGitLines = fmap (map unpack . lines) . readGit

-- | An instance where all operations no-op or return empty strings
newtype NullGit m a = NullGit
  { unwrap :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

instance Monad m => MonadGit (NullGit m) where
  gitDiffNameOnly _ = pure []
  gitCommitAll _ = pure ""
