-- |
--
-- Module      : Restyler.Monad.ReadFile
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Monad.ReadFile
  ( MonadReadFile (..)

    -- * DerivingVia
  , ActualReadFile (..)
  ) where

import Restyler.Prelude

import Relude qualified as Prelude

class Monad m => MonadReadFile m where
  readFileBS :: forall b. Path b File -> m ByteString
  readFileBS = fmap encodeUtf8 . readFile

  readFile :: forall b. Path b File -> m Text
  readFile = fmap (decodeUtf8With lenientDecode) . readFileBS
  {-# MINIMAL readFileBS | readFile #-}

newtype ActualReadFile m a = ActualReadFile
  { unwrap :: m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadLogger
    )

instance (MonadIO m, MonadLogger m) => MonadReadFile (ActualReadFile m) where
  readFileBS path = do
    logTrace $ "readFileBS" :# ["path" .= path]
    Prelude.readFileBS $ toFilePath path
