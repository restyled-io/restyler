module Restyler.Monad.WriteFile
  ( MonadWriteFile (..)

    -- * DerivingVia
  , ActualWriteFile (..)
  ) where

import Restyler.Prelude

import Relude qualified as Prelude

class Monad m => MonadWriteFile m where
  writeFile :: FilePath -> Text -> m ()

newtype ActualWriteFile m a = ActualWriteFile
  { unwrap :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadLogger
    )

instance (MonadIO m, MonadLogger m) => MonadWriteFile (ActualWriteFile m) where
  writeFile path contents = do
    logTrace $ "writeFile" :# ["path" .= path]
    Prelude.writeFileText path contents
