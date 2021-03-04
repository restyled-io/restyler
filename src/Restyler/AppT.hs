{-# LANGUAGE DerivingVia #-}

module Restyler.AppT
    ( AppT(..)
    , runAppT
    , replaceAppT
    , appErrorBoundary
    ) where

import Restyler.Prelude

import Control.Monad.Reader
import Restyler.App.Error
import Restyler.Capabilities.DownloadFile
import Restyler.Capabilities.Git
import Restyler.Capabilities.GitHub
import Restyler.Capabilities.Hushed
import Restyler.Capabilities.Logger
import Restyler.Capabilities.Process
import Restyler.Capabilities.System

newtype AppT env a = AppT
    { unAppT :: ReaderT env (ExceptT AppError IO) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadUnliftIO
        , MonadReader env
        , MonadError AppError
        )
    deriving MonadLogger via (ActualLogger (AppT env))
    deriving MonadHushed via (ActualHushed (AppT env))
    deriving MonadSystem via (ActualSystem (AppT env))
    deriving MonadProcess via (ActualProcess (AppT env))
    deriving MonadDownloadFile via (ActualDownloadFile (AppT env))
    deriving MonadGit via (ActualGit (AppT env))
    deriving MonadGitHub via (ActualGitHub (AppT env))

runAppT :: MonadIO m => env -> AppT env a -> m (Either AppError a)
runAppT app = liftIO . runExceptT . flip runReaderT app . unAppT

replaceAppT :: env -> AppT env a -> AppT env' a
replaceAppT app = withAppT (const app)

withAppT :: (env' -> env) -> AppT env a -> AppT env' a
withAppT f = AppT . withReaderT f . unAppT

appErrorBoundary :: AppT env a -> AppT env a
appErrorBoundary = handle $ throwError . OtherError
