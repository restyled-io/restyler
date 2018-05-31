module Restyler.Capabilities.System
    ( MonadSystem(..)
    )
where

import Restyler.Prelude

class MonadSystem m where
    doesFileExist :: FilePath -> m Bool
    getCurrentDirectory :: m FilePath
    setCurrentDirectory :: FilePath -> m ()
    readFile :: FilePath -> m Text
    exitSuccess :: m a
