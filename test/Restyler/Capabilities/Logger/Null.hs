module Restyler.Capabilities.Logger.Null
    ( NullLogger(..)
    ) where

import Restyler.Prelude

import Restyler.Capabilities.Logger

newtype NullLogger m a = NullLogger
    { unNullLogger :: m a
    }
    deriving newtype (Applicative, Functor, Monad)

instance Monad m => MonadLogger (NullLogger m) where
    logDebug _ = pure ()
    logInfo _ = pure ()
    logWarn _ = pure ()
    logError _ = pure ()
    logOther _ _ = pure ()
    logSticky _ = pure ()
    logStickyDone _ = pure ()
