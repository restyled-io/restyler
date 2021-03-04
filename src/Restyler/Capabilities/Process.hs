module Restyler.Capabilities.Process
    ( MonadProcess(..)
    , ActualProcess(..)
    ) where

import Restyler.Prelude

import qualified UnliftIO.Process as Process

class Monad m => MonadProcess m where
    callProcess :: String -> [String] -> m ()
    callProcessExitCode :: String -> [String] -> m ExitCode
    readProcess :: String -> [String] -> String -> m String

instance MonadProcess m => MonadProcess (ExceptT e m) where
    callProcess x = lift . callProcess x
    callProcessExitCode x = lift . callProcessExitCode x
    readProcess x y = lift . readProcess x y

newtype ActualProcess m a = ActualProcess
    { unActualProcess :: m a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadUnliftIO m => MonadUnliftIO (ActualProcess m) where
    {-# INLINE withRunInIO #-}
    withRunInIO inner =
        ActualProcess $ withRunInIO $ \run -> inner (run . unActualProcess)

instance MonadUnliftIO m => MonadProcess (ActualProcess m) where
    callProcess = Process.callProcess
    callProcessExitCode cmd args = Process.withCreateProcess proc
        $ \_ _ _ p -> Process.waitForProcess p
        where proc = (Process.proc cmd args) { Process.delegate_ctlc = True }
    readProcess = Process.readProcess

-- actualProcess
--     :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => Process m
-- actualProcess = Process
--     { callProcess = \cmd args -> do
--         logDebug $ "call: " <> fromString cmd <> " " <> displayShow args
--         appIO SystemError $ Process.callProcess cmd args
--     , callProcessExitCode = \cmd args -> do
--         let proc = (Process.proc cmd args) { Process.delegate_ctlc = True }
--         logDebug $ "call: " <> fromString cmd <> " " <> displayShow args
--         ec <- appIO SystemError $ Process.withCreateProcess proc $ \_ _ _ p ->
--             Process.waitForProcess p
--         ec <$ logDebug ("exit code: " <> displayShow ec)
--     , readProcess = \cmd args stdin' -> do
--         logDebug $ "read: " <> fromString cmd <> " " <> displayShow args
--         output <- appIO SystemError $ Process.readProcess cmd args stdin'
--         output <$ logDebug ("output: " <> fromString output)
--     }
