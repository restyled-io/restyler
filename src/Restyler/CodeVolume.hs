-- |
--
-- Module      : Restyler.CodeVolume
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.CodeVolume
  ( withCodeVolume
  ) where

import Restyler.Prelude

import Restyler.Monad.Docker
import UnliftIO.Exception (bracket)

withCodeVolume :: (MonadDocker m, MonadUnliftIO m) => (String -> m a) -> m a
withCodeVolume = bracket acquire dockerVolumeRm
 where
  acquire :: (MonadDocker m, MonadUnliftIO m) => m String
  acquire = withVolumeInContainer tmpVolumeName $ \cName cPath -> do
    tmpVolumeName <$ dockerCp "." (cName <> ":" <> cPath)

withVolumeInContainer
  :: (MonadDocker m, MonadUnliftIO m)
  => String
  -> (String -> FilePath -> m a)
  -> m a
withVolumeInContainer name = bracket acquire (dockerRm . fst) . uncurry
 where
  acquire :: MonadDocker m => m (String, FilePath)
  acquire = do
    dockerCreate
      $ ["--name", tmpContainerName]
      <> ["--volume", name <> ":" <> tmpContainerPath]
      <> ["busybox"]

    pure (tmpContainerName, tmpContainerPath)

tmpVolumeName :: String
tmpVolumeName = "restyler-code-volume"

tmpContainerName :: String
tmpContainerName = "restyler-tmp-container"

tmpContainerPath :: FilePath
tmpContainerPath = "/data"
