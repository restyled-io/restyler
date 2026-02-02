-- |
--
-- Module      : Restyler.CodeVolume
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.CodeVolume
  ( VolumeName (..)
  , withCodeVolume
  ) where

import Restyler.Prelude

import Restyler.Monad.Docker
import UnliftIO.Exception (bracket)

newtype VolumeName = VolumeName
  { unwrap :: String
  }

withCodeVolume :: (MonadDocker m, MonadUnliftIO m) => (VolumeName -> m a) -> m a
withCodeVolume = bracket acquire (dockerVolumeRm . (.unwrap))
 where
  acquire :: (MonadDocker m, MonadUnliftIO m) => m VolumeName
  acquire = withVolumeInContainer tmpVolumeName $ \cName cPath -> do
    tmpVolumeName <$ dockerCp "." (cName.unwrap <> ":" <> cPath.unwrap)

newtype ContainerName = ContainerName
  { unwrap :: String
  }

newtype ContainerPath = ContainerPath
  { unwrap :: FilePath
  }

withVolumeInContainer
  :: (MonadDocker m, MonadUnliftIO m)
  => VolumeName
  -> (ContainerName -> ContainerPath -> m a)
  -> m a
withVolumeInContainer name =
  bracket acquire (dockerRm . (.unwrap) . fst) . uncurry
 where
  acquire :: MonadDocker m => m (ContainerName, ContainerPath)
  acquire = do
    dockerCreate
      $ ["--name", tmpContainerName.unwrap]
      <> ["--volume", name.unwrap <> ":" <> tmpContainerPath.unwrap]
      <> ["busybox"]

    pure (tmpContainerName, tmpContainerPath)

tmpVolumeName :: VolumeName
tmpVolumeName = VolumeName "restyler-code-volume"

tmpContainerName :: ContainerName
tmpContainerName = ContainerName "restyler-tmp-container"

tmpContainerPath :: ContainerPath
tmpContainerPath = ContainerPath "/data"
