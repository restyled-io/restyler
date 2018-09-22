module Restyler.App.Run
    ( runApp
    ) where

import Restyler.Prelude

import Restyler.App.Type
import Restyler.Logger

-- | Run an @'AppT'@ action for real
--
-- N.B. This unwraps only as far as @'ExceptT'@ so that it can be composed
-- together with @'bootstrapApp'@ and receive overall error-handling after.
--
-- See @"Restyler.Main"@.
--
runApp :: MonadIO m => App -> AppT m a -> ExceptT AppError m a
runApp app = runAppLoggingT app . flip runReaderT app . runAppT
