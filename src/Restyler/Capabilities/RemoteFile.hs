module Restyler.Capabilities.RemoteFile
    ( MonadRemoteFile(..)
    ) where

import Restyler.Model.RemoteFile

class MonadRemoteFile m where
    fetchRemoteFile :: RemoteFile -> m ()
