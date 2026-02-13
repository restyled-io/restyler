{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- Module      : Restyler.Monad.Docker
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Monad.Docker
  ( MonadDocker (..)

    -- * @DerivingVia@
  , ActualDocker (..)
  , NullDocker (..)
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.AnnotatedException
import System.Process.Typed

class Monad m => MonadDocker m where
  dockerPull :: HasCallStack => String -> m ExitCode
  dockerCreate :: HasCallStack => [String] -> m ()
  dockerRun :: HasCallStack => [String] -> m ExitCode
  dockerRunStdout :: HasCallStack => [String] -> m (ExitCode, Text)
  dockerImageRm :: HasCallStack => String -> m ()
  dockerVolumeCreate :: HasCallStack => String -> m ()
  dockerVolumeRm :: HasCallStack => String -> m ()
  dockerCp :: String -> String -> m ()
  dockerRm :: String -> m ()

-- | An instance that invokes the real @docker@
newtype ActualDocker m a = ActualDocker
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
  => MonadDocker (ActualDocker m)
  where
  dockerPull image = runDocker ["pull", "--quiet", image]
  dockerCreate args = runDocker_ $ ["create", "--quiet"] <> args
  dockerRun args = runDocker $ ["run"] <> args
  dockerRunStdout args = runDockerStdout $ ["run"] <> args
  dockerImageRm image = runDocker_ ["image", "rm", "--force", image]
  dockerVolumeCreate name = runDocker_ ["volume", "create", name]
  dockerVolumeRm name = runDocker_ ["volume", "rm", name]
  dockerCp src dst = runDocker_ ["cp", "--quiet", src, dst]
  dockerRm name = runDocker_ ["rm", name]

runDocker
  :: (HasCallStack, HasLogger env, MonadLogger m, MonadReader env m, MonadUnliftIO m)
  => [String]
  -> m ExitCode
runDocker args = checkpointCallStack $ do
  logProc "docker" args
  runProcess $ proc "docker" args

runDocker_
  :: (HasCallStack, HasLogger env, MonadLogger m, MonadReader env m, MonadUnliftIO m)
  => [String]
  -> m ()
runDocker_ args = checkpointCallStack $ do
  logProc "docker" args
  runProcess_ $ proc "docker" args

runDockerStdout
  :: (HasCallStack, HasLogger env, MonadLogger m, MonadReader env m, MonadUnliftIO m)
  => [String]
  -> m (ExitCode, Text)
runDockerStdout args = checkpointCallStack $ do
  logProc "docker" args
  second (fixNewline . decodeUtf8) <$> readProcessStdout (proc "docker" args)

fixNewline :: Text -> Text
fixNewline = (<> "\n") . T.dropWhileEnd (== '\n')

-- | An instance where all operations no-op or return empty strings
newtype NullDocker m a = NullDocker
  { unwrap :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

instance Monad m => MonadDocker (NullDocker m) where
  dockerPull _ = pure ExitSuccess
  dockerCreate _ = pure ()
  dockerRun _ = pure ExitSuccess
  dockerRunStdout _ = pure (ExitSuccess, "")
  dockerImageRm _ = pure ()
  dockerVolumeCreate _ = pure ()
  dockerVolumeRm _ = pure ()
  dockerCp _ _ = pure ()
  dockerRm _ = pure ()
