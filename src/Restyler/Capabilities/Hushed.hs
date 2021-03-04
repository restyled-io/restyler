module Restyler.Capabilities.Hushed
    ( MonadHushed(..)
    , hushed_
    , ActualHushed(..)
    ) where

import Restyler.Prelude

import Restyler.Capabilities.Logger

class Monad m => MonadHushed m where
    hushed :: m a -> m (Maybe a)

instance MonadHushed m => MonadHushed (ExceptT e m) where
    hushed f = ExceptT $ fmap x $ hushed $ runExceptT f
      where
        x :: Maybe (Either e a) -> Either e (Maybe a)
        x = \case
            Nothing -> Right Nothing
            Just (Left e) -> Left e
            Just (Right a) -> Right (Just a)

hushed_ :: MonadHushed m => m a -> m ()
hushed_ = void . hushed

newtype ActualHushed m a = ActualHushed
    { unActualHushed :: m a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance MonadUnliftIO m => MonadUnliftIO (ActualHushed m) where
    {-# INLINE withRunInIO #-}
    withRunInIO inner =
        ActualHushed $ withRunInIO $ \run -> inner (run . unActualHushed)

instance (MonadUnliftIO m, MonadLogger m) => MonadHushed (ActualHushed m) where
    hushed f = handleAny warnIgnore $ Just <$> f

warnIgnore :: (MonadLogger m, Display ex) => ex -> m (Maybe a)
warnIgnore ex = do
    logWarn $ "Exception silenced: " <> display ex
    pure Nothing
