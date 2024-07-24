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

    -- * Annotated-safe handling
  , handleTo
  , tryTo

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

handleTo
  :: (MonadUnliftIO m, Exception e1, Exception e2) => (e1 -> e2) -> m a -> m a
handleTo f action = do
  r <- tryAnnotated action
  case r of
    Left AnnotatedException {exception} -> throw $ f exception
    Right a -> pure a

tryTo :: (MonadUnliftIO m, Exception e) => (e -> b) -> m a -> m (Either b a)
tryTo f = fmap (first $ f . exception) . tryAnnotated

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
