-- |
--
-- Module      : Restyler.AnnotatedException
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.AnnotatedException
  ( checkpointCallStack
  , AnnotatedException (..)
  , throw
  , tryAnnotated
  , Handler (..)
  , catch
  , check
  , hide
  , displayAnnotatedException

    -- * Suppressing exception
  , suppressWith
  , suppressWarn
  , suppressWarnWith
  ) where

import Restyler.Prelude

import Control.Exception.Annotated.UnliftIO

displayAnnotatedException :: Exception e => AnnotatedException e -> Text
displayAnnotatedException aex@AnnotatedException {exception} =
  unlines
    [ pack $ displayException exception
    , ""
    , maybe "" (pack . prettyCallStack) $ annotatedExceptionCallStack aex
    ]

-- | Suppress any exception and return the given @a@
suppressWith :: MonadUnliftIO m => a -> m a -> m a
suppressWith x = either (const $ pure x) pure <=< tryAnnotated @SomeException

-- | Log any exception and return '()'
suppressWarn :: (MonadUnliftIO m, MonadLogger m) => m () -> m ()
suppressWarn = suppressWarnWith ()

-- | Log any exception and return the given @a@
suppressWarnWith :: (MonadUnliftIO m, MonadLogger m) => a -> m a -> m a
suppressWarnWith x = either warn pure <=< tryAnnotated @SomeException
 where
  warn ex = do
    logWarn $ ("Suppressed " <> displayAnnotatedException ex) :# []
    pure x
