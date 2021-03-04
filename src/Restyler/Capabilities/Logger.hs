{-# LANGUAGE UndecidableInstances #-}

module Restyler.Capabilities.Logger
    ( MonadLogger(..)
    , ActualLogger(..)
    ) where

import Restyler.Prelude

import qualified RIO

class Monad m => MonadLogger m where
    logDebug :: Utf8Builder -> m ()
    logInfo :: Utf8Builder -> m ()
    logWarn :: Utf8Builder -> m ()
    logError :: Utf8Builder -> m ()
    logOther :: Text -> Utf8Builder -> m ()
    logSticky :: Utf8Builder -> m ()
    logStickyDone :: Utf8Builder -> m ()

newtype ActualLogger m a = ActualLogger
    { unActualLogger :: m a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader env)

instance MonadLogger m => MonadLogger (ExceptT e m) where
    logDebug = lift . logDebug
    logInfo = lift . logInfo
    logWarn = lift . logWarn
    logError = lift . logError
    logOther x = lift . logOther x
    logSticky = lift . logSticky
    logStickyDone = lift . logStickyDone

instance (MonadIO m, MonadReader env m, HasLogFunc env)
    => MonadLogger (ActualLogger m) where
    logDebug = RIO.logDebug
    logInfo = RIO.logInfo
    logWarn = RIO.logWarn
    logError = RIO.logError
    logOther = RIO.logOther
    logSticky = RIO.logSticky
    logStickyDone = RIO.logStickyDone
