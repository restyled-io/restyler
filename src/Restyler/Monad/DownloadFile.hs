module Restyler.Monad.DownloadFile
  ( MonadDownloadFile (..)

    -- * DerivingVia
  , ActualDownloadFile (..)
  , NullDownloadFile (..)
  ) where

import Restyler.Prelude

import Conduit (runResourceT, sinkFile)
import Network.HTTP.Simple hiding (Request)

class Monad m => MonadDownloadFile m where
  downloadFile :: String -> FilePath -> m ()

newtype ActualDownloadFile m a = ActualDownloadFile
  { unwrap :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    )

instance
  (MonadUnliftIO m, MonadLogger m)
  => MonadDownloadFile (ActualDownloadFile m)
  where
  downloadFile url path = do
    logDebug $ "downloadFile" :# ["url" .= url]
    liftIO $ do
      request <- parseRequestThrow url
      runResourceT $ httpSink request $ \_ -> sinkFile path

newtype NullDownloadFile m a = NullDownloadFile
  { unwrap :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    )

instance Monad m => MonadDownloadFile (NullDownloadFile m) where
  downloadFile _url _path = pure ()
