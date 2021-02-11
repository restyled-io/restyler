{-# LANGUAGE UndecidableInstances #-}

module Restyler.Capabilities.Hushed.Except
    ( ExceptHushed(..)
    )
where

import Restyler.Prelude

import Restyler.Capabilities.Hushed

newtype ExceptHushed m a = ExceptHushed
    { unExceptHushed :: m a
    }
    deriving newtype (Applicative, Functor, Monad, MonadError e)

instance MonadError e m => MonadHushed (ExceptHushed m) where
    hushed f = (Just <$> f) `catchError` const (pure Nothing)
