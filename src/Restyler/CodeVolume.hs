{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- Module      : Restyler.CodeVolume
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.CodeVolume
  ( CodeVolume (..)
  , VolumeName (..)
  , ContainerName (..)
  , ContainerPath (..)
  , withCodeVolume
  ) where

import Restyler.Prelude

import Path (absdir)
import Restyler.Monad.Docker
import UnliftIO.Exception (bracket)

data CodeVolume = CodeVolume
  { name :: VolumeName
  , container :: ContainerName
  , path :: ContainerPath
  }

newtype VolumeName = VolumeName
  { unwrap :: String
  }

newtype ContainerName = ContainerName
  { unwrap :: String
  }

newtype ContainerPath = ContainerPath
  { unwrap :: Path Abs Dir
  }

withCodeVolume :: (MonadDocker m, MonadUnliftIO m) => (CodeVolume -> m a) -> m a
withCodeVolume = bracket acquire release
 where
  acquire :: MonadDocker m => m CodeVolume
  acquire = do
    let
      vol :: CodeVolume
      vol =
        CodeVolume
          { name = VolumeName "restyler-code-volume"
          , container = ContainerName "restyler-tmp-container"
          , path = ContainerPath [absdir|/data|]
          }

      mount :: String
      mount = vol.name.unwrap <> ":" <> toFilePath vol.path.unwrap

    dockerVolumeCreate vol.name.unwrap
    dockerCreate ["--name", vol.container.unwrap, "--volume", mount, "busybox"]
    pure vol

  release :: MonadDocker m => CodeVolume -> m ()
  release vol = do
    dockerRm vol.container.unwrap
    dockerVolumeRm vol.name.unwrap
