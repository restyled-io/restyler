module Restyler.Exit
  ( withExitHandler
  ) where

import Restyler.Prelude

import Blammo.Logging.Logger (flushLogger)
import Data.Text.IO qualified as T
import Restyler.AnnotatedException
import Restyler.Error

withExitHandler
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasCallStack
     )
  => m a
  -> m a
withExitHandler = (`catch` exitHandler)

exitHandler
  :: ( MonadIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasCallStack
     )
  => AnnotatedException SomeException
  -> m a
exitHandler aex = do
  err <- runErrorHandlers aex

  case err.severity of
    "warning" -> logWarn err.message
    _ -> logError err.message

  flushLogger
  liftIO $ T.hPutStrLn stderr $ displayAnnotatedException aex
  exitWith err.exitCode
