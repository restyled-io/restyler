module Restyler.Exit
  ( withExitHandler
  ) where

import Restyler.Prelude

import Restyler.AnnotatedException
import Restyler.ErrorMetadata

withExitHandler :: (MonadUnliftIO m, MonadLogger m) => m a -> m ()
withExitHandler f = do
  ec <- (ExitSuccess <$ f) `catches` exitHandlers
  exitWith ec

exitHandlers :: (MonadIO m, MonadLogger m) => [Handler m ExitCode]
exitHandlers = map toExitHandler errorHandlers

toExitHandler
  :: (MonadIO m, MonadLogger m) => Handler m ErrorMetadata -> Handler m ExitCode
toExitHandler (Handler f) = Handler $ \ex -> do
  md <- f ex
  case md.severity of
    "warning" -> logWarn md.message
    _ -> logError md.message
  pure md.exitCode
