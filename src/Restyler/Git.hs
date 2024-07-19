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
  gitPush branch = runGit_ ["push", "origin", branch]
  gitPushForce branch = runGit_ ["push", "--force", "origin", branch]
  gitDiffNameOnly mRef = readGitLines $ ["diff", "--name-only"] <> maybeToList mRef
  gitFormatPatch mRef = readGit $ ["format-patch", "--stdout"] <> maybeToList mRef
  gitCommitAll msg = do
    runProcess_ $ proc "git" ["commit", "-a", "--message", msg]
    readGitChomp ["rev-parse", "HEAD"]
  gitCheckout branch = runGit_ ["checkout", "--no-progress", "-b", branch]
  gitInit = runGit_ ["init", "--quiet", "."]
  gitRemoteAdd name url = runGit_ ["remote", "add", name, url]
  gitFetch name refspec = runGit_ ["fetch", "--quiet", "--depth", "1", name, refspec]
  gitSwitch branch = runGit_ ["checkout", "--no-progress", branch]

runGit_
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasCallStack
     )
  => [String]
  -> m ()
runGit_ args = checkpointCallStack $ do
  logDebug $ ("exec git " <> unwords (map (sanitizeToken . pack) args)) :# []
  flushLogger
  runProcess_ $ proc "git" args

-- | Best-effort sanitize of arguments that contain a GitHub token
--
-- If this doesn't work, it's fine. The logging is as DEBUG and there is more
-- robust sanitization wherever these logs will appear (restyled.io or GHA).
sanitizeToken :: Text -> Text
sanitizeToken original = fromMaybe original $ do
  rest <- T.stripPrefix "https://x-access-token:" original
  pure $ "https://" <> T.drop 1 (T.dropWhile (/= '@') rest)

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
