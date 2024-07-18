module Restyler.Exit
  ( withExitHandler
  ) where

import Restyler.Prelude

import Restyler.AnnotatedException
import Restyler.Error

withExitHandler :: (MonadUnliftIO m, MonadLogger m) => m a -> m ()
withExitHandler f = do
  ec <- (ExitSuccess <$ f) `catches` exitHandlers
  exitWith ec

exitHandlers :: (MonadIO m, MonadLogger m) => [Handler m ExitCode]
exitHandlers = map toExitHandler errorHandlers

toExitHandler
  :: (MonadIO m, MonadLogger m) => Handler m Error -> Handler m ExitCode
toExitHandler (Handler f) = Handler $ \ex -> do
  err <- f ex
  case err.severity of
    "warning" -> logWarn err.message
    _ -> logError err.message
  pure err.exitCode
