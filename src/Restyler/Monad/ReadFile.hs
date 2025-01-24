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
  readFileBS :: FilePath -> m ByteString
  readFileBS = fmap encodeUtf8 . readFile

  readFile :: FilePath -> m Text
  readFile = fmap (decodeUtf8With lenientDecode) . readFileBS
  {-# MINIMAL readFileBS | readFile #-}

newtype ActualReadFile m a = ActualReadFile
  { unwrap :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadLogger
    )

instance (MonadIO m, MonadLogger m) => MonadReadFile (ActualReadFile m) where
  readFileBS path = do
    logTrace $ "readFileBS" :# ["path" .= path]
    Prelude.readFileBS path
