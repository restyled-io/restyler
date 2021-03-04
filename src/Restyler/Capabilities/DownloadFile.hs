{-# LANGUAGE UndecidableInstances #-}

module Restyler.Capabilities.DownloadFile
    ( MonadDownloadFile(..)
    , ActualDownloadFile(..)
    ) where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import Network.HTTP.Simple hiding (Request)
import Restyler.App.Error
import Restyler.Capabilities.Logger

class Monad m => MonadDownloadFile m where
    downloadFile :: Text -> FilePath -> m ()

instance MonadDownloadFile m => MonadDownloadFile (ExceptT e m) where
    downloadFile x = lift . downloadFile x

newtype ActualDownloadFile m a = ActualDownloadFile
    { unActualDownloadFile :: m a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadLogger
        , MonadError e
        )

instance MonadUnliftIO m => MonadUnliftIO (ActualDownloadFile m) where
    {-# INLINE withRunInIO #-}
    withRunInIO inner = ActualDownloadFile $ withRunInIO $ \run ->
        inner (run . unActualDownloadFile)

instance (MonadUnliftIO m, MonadLogger m, MonadError AppError m)
    => MonadDownloadFile (ActualDownloadFile m) where
    downloadFile url path = handle (throwError . HttpError) $ do
        logInfo $ "Download " <> display url <> " => " <> displayShow path
        request <- either throwInvalidUrl pure $ parseRequestThrow $ unpack url
        runResourceT $ httpSink request $ \_ -> sinkFile path

throwInvalidUrl :: MonadError AppError m => SomeException -> m a
throwInvalidUrl ex = case fromException ex of
    Nothing -> throwError $ OtherError ex
    Just x -> throwError $ HttpError x
