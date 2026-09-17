-- |
--
-- Module      : Restyler.Config.CopyFiles
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.CopyFiles
  ( HasCopyFiles (..)
  , CopyFiles (..)
  , copyFilesParser
  , copyCodeFiles
  ) where

import Restyler.Prelude

import Autodocodec hiding ((.=))
import OptEnvConf hiding (env)
import Restyler.CodeVolume
import Restyler.Config.Glob
import Restyler.Config.RemoteFile
import Restyler.Monad.Directory
import Restyler.Monad.Docker

class HasCopyFiles env where
  getCopyFiles :: env -> CopyFiles

newtype CopyFiles = CopyFiles
  { unwrap :: [Glob FilePath]
  }
  deriving stock (Eq)
  deriving newtype (HasCodec, Show)

copyFilesParser :: Parser CopyFiles
copyFilesParser =
  setting
    [ help "Files to include into restyling context"
    , example
        $ unpack
        $ unlines
          [ "# copy the entire current directory (default)"
          , "copy_files: [\".\"]"
          ]
    , example
        $ unpack
        $ unlines
          [ "# copy only the files being restyled"
          , "copy_files: []"
          ]
    , example
        $ unpack
        $ unlines
          [ "# copy only the files being restyled and an explicit list"
          , "copy_files:"
          , "  - *.cabal"
          , "  - .prettierrc"
          ]
    , conf "copy_files"
    , value $ CopyFiles ["."]
    ]

copyCodeFiles
  :: ( HasCopyFiles env
     , MonadDirectory m
     , MonadDocker m
     , MonadLogger m
     , MonadReader env m
     )
  => [RemoteFile]
  -- ^ Downloaded remote files
  --
  -- Presumably you'd want them in the context, otherwise why download them?
  -> [Path Rel File]
  -- ^ Files to restyle
  --
  -- These must also always be in context.
  -> CodeVolume
  -> m ()
copyCodeFiles remoteFiles paths vol = do
  asks getCopyFiles >>= \case
    CopyFiles [] -> do
      logDebug "Copying no extra paths into code volume"
      dockerCpAll alwaysPaths
    CopyFiles ["."] -> do
      logDebug "Copying all of . into code volume"
      dockerCpDot
    CopyFiles gs | "." `elem` gs -> do
      logDebug
        $ "Copying all of . into code volume (ignoring all other globs)"
        :# ["globs" .= gs]
      dockerCpDot
    CopyFiles gs -> do
      logDebug $ "Copying explicit paths into code volume" :# ["globs" .= gs]
      ps <- globAnyInCurrentDirectory gs
      dockerCpAll $ alwaysPaths <> ps
 where
  alwaysPaths = map (.path) remoteFiles <> paths

  dockerCpDot = do
    -- docker cp . container:/path
    dockerCp "." $ vol.container.unwrap <> ":" <> toFilePath vol.path.unwrap

  dockerCpAll ps =
    -- tar -cf - ps | docker cp - container:/path
    dockerCpTar ps $ vol.container.unwrap <> ":" <> toFilePath vol.path.unwrap
