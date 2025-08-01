-- |
--
-- Module      : Restyler.Test.App
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Test.App
  ( TestAppT (..)
  , SimpleTestApp (..)
  , withSimpleTestApp
  , shouldBe
  , shouldReturn
  , module Test.Hspec
  , module X
  ) where

import Restyler.Prelude

import Blammo.Logging.LogSettings (defaultLogSettings)
import Blammo.Logging.Logger (newTestLogger)
import Restyler.Monad.Directory as X
import Restyler.Monad.Docker as X
import Restyler.Monad.DownloadFile as X
import Restyler.Monad.Git as X
import Restyler.Monad.ReadFile as X
import Restyler.Monad.WriteFile as X
import Restyler.Test.FS (FS, HasFS (..), ReaderFS (..))
import Restyler.Test.FS qualified as FS
import Test.Hspec hiding (shouldBe, shouldReturn)
import Test.Hspec.Core.Spec (Example (..))
import Test.Hspec.Expectations.Lifted qualified as Lifted

newtype TestAppT env a = TestAppT
  { unwrap :: ReaderT env IO a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader env
    , MonadUnliftIO
    )
  deriving (MonadLogger, MonadLoggerIO) via (WithLogger env IO)
  deriving (MonadDirectory) via (ReaderFS (TestAppT env))
  deriving (MonadDocker) via (NullDocker (TestAppT env))
  deriving (MonadDownloadFile) via (NullDownloadFile (TestAppT env))
  deriving (MonadGit) via (NullGit (TestAppT env))
  deriving (MonadReadFile) via (ReaderFS (TestAppT env))
  deriving (MonadWriteFile) via (ReaderFS (TestAppT env))

instance Example (TestAppT env a) where
  type Arg (TestAppT env a) = env

  evaluateExample f params action =
    evaluateExample
      (action $ \env -> void $ runReaderT f.unwrap env)
      params
      ($ ())

-- | A simple test app, with a filesystem and logger
data SimpleTestApp = SimpleTestApp
  { logger :: Logger
  , fs :: FS
  }

instance HasLogger SimpleTestApp where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasFS SimpleTestApp where
  fsL = lens (.fs) $ \x y -> x {fs = y}

withSimpleTestApp :: SpecWith SimpleTestApp -> Spec
withSimpleTestApp =
  before
    $ SimpleTestApp
    <$> newTestLogger defaultLogSettings
    <*> FS.build "/" []

shouldBe :: (Eq a, HasCallStack, Show a) => a -> a -> TestAppT env ()
shouldBe = Lifted.shouldBe

infix 1 `shouldBe`

shouldReturn
  :: (Eq a, HasCallStack, Show a) => TestAppT env a -> a -> TestAppT env ()
shouldReturn = Lifted.shouldReturn

infix 1 `shouldReturn`
