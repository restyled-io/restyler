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

data CopyFiles
  = CopyAll
  | CopyNone
  | CopyOnly (NonEmpty (Glob FilePath))
  deriving stock (Eq, Show)

instance HasCodec CopyFiles where
  codec = dimapCodec toCopyFiles fromCopyFiles $ maybeCodec codec

toCopyFiles :: Maybe [Glob FilePath] -> CopyFiles
toCopyFiles = \case
  Nothing -> CopyAll
  Just gs -> maybe CopyNone CopyOnly $ nonEmpty gs

fromCopyFiles :: CopyFiles -> Maybe [Glob FilePath]
fromCopyFiles = \case
  CopyAll -> Nothing
  CopyNone -> Just []
  CopyOnly negs -> Just $ toList negs

copyFilesParser :: Parser CopyFiles
copyFilesParser =
  setting
    [ help
        $ unpack
        $ unlines
          [ "Files to include into restyling context"
          ]
    , example
        $ unpack
        $ unlines
          [ "# copy the entire current directory (default)"
          , "copy_files: null"
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
    , value CopyAll
    ]

copyCodeFiles
  :: (MonadDirectory m, MonadDocker m, MonadLogger m)
  => [RemoteFile]
  -- ^ Downloaded remote files
  --
  -- Presumably you'd want them in the context, otherwise why download them?
  -> [Path Rel File]
  -- ^ Files to restyle
  --
  -- These must also always be in context.
  -> CodeVolume
  -> CopyFiles
  -> m ()
copyCodeFiles remoteFiles paths vol = \case
  CopyAll -> do
    logDebug "Copying all of . into code volume"
    dockerCpDot
  CopyNone -> do
    logDebug "Copying no extra paths into code volume"
    dockerCpAll alwaysPaths
  CopyOnly gs -> do
    logDebug $ "Copying explicit paths into code volume" :# ["globs" .= gs]
    ps <- globAnyInCurrentDirectory $ toList gs
    dockerCpAll $ alwaysPaths <> ps
 where
  alwaysPaths = map (.path) remoteFiles <> paths

  dockerCpDot = do
    -- docker cp . container:/path
    dockerCp "." $ vol.container.unwrap <> ":" <> toFilePath vol.path.unwrap

  dockerCpAll ps =
    -- tar -cf - ps | docker cp - container:/path
    dockerCpTar ps $ vol.container.unwrap <> ":" <> toFilePath vol.path.unwrap
