-- |
--
-- Module      : Restyler.Monad.DownloadFile
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
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
  downloadFile :: String -> Path b File -> m ()

newtype ActualDownloadFile m a = ActualDownloadFile
  { unwrap :: m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadLogger
    , MonadUnliftIO
    )

instance
  (MonadLogger m, MonadUnliftIO m)
  => MonadDownloadFile (ActualDownloadFile m)
  where
  downloadFile url path = do
    logDebug $ "downloadFile" :# ["url" .= url]
    liftIO $ do
      request <- parseRequestThrow url
      runResourceT $ httpSink request $ \_ -> sinkFile (toFilePath path)

newtype NullDownloadFile m a = NullDownloadFile
  { unwrap :: m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    )

instance Monad m => MonadDownloadFile (NullDownloadFile m) where
  downloadFile _url _path = pure ()
