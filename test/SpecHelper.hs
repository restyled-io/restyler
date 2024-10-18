{-# LANGUAGE FieldSelectors #-}

-- |
--
-- Module      : SpecHelper
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module SpecHelper
  ( someRestyler

    -- * TestApp
  , TestApp (..)
  , TestAppT (..)
  , withTestApp
  , testAppExample

    -- * Re-exports
  , module X
  , shouldThrow
  ) where

import Restyler.Monad.Directory as X
import Restyler.Monad.ReadFile as X
import Restyler.Monad.WriteFile as X
import Restyler.Prelude as X
import Test.Hspec as X hiding
  ( expectationFailure
  , pendingWith
  , shouldBe
  , shouldContain
  , shouldEndWith
  , shouldMatchList
  , shouldNotBe
  , shouldNotContain
  , shouldNotReturn
  , shouldNotSatisfy
  , shouldReturn
  , shouldSatisfy
  , shouldStartWith
  , shouldThrow
  )
import Test.Hspec.Expectations.Lifted as X
import Test.QuickCheck as X

import Blammo.Logging.Simple
import Data.Typeable (typeOf)
import LoadEnv (loadEnvFrom)
import Restyler.AnnotatedException
import Restyler.Config
import Restyler.Monad.Docker
import Restyler.Monad.DownloadFile
import Restyler.Monad.Git
import Restyler.Restyler
import Restyler.Test.FS (FS, HasFS (..), ReaderFS (..))
import Restyler.Test.FS qualified as FS
import Test.Hspec.Core.Spec (Example (..))

data TestApp = TestApp
  { taLogger :: Logger
  , taFS :: FS
  , taDockerPullExitCode :: ExitCode
  , taDockerRunExitCode :: ExitCode
  }

instance HasLogger TestApp where
  loggerL = lens taLogger $ \x y -> x {taLogger = y}

instance HasCommitTemplate TestApp where
  getCommitTemplate _ = CommitTemplate ""

instance HasDryRun TestApp where getDryRun _ = False
instance HasHostDirectory TestApp where
  getHostDirectory _ = error "hostDirectory"
instance HasImageCleanup TestApp where getImageCleanup _ = False
instance HasNoCommit TestApp where getNoCommit _ = False
instance HasNoPull TestApp where getNoPull _ = False
instance HasRestrictions TestApp where getRestrictions _ = error "restrictions"

instance HasFS TestApp where
  fsL = lens taFS $ \x y -> x {taFS = y}

newtype TestAppT a = TestAppT
  { unTestAppT :: ReaderT TestApp (LoggingT IO) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    , MonadReader TestApp
    )
  deriving (MonadDirectory) via (ReaderFS TestAppT)
  deriving (MonadDownloadFile) via (NullDownloadFile TestAppT)
  deriving (MonadGit) via (NullGit TestAppT)
  deriving (MonadReadFile) via (ReaderFS TestAppT)
  deriving (MonadWriteFile) via (ReaderFS TestAppT)

instance MonadDocker TestAppT where
  dockerPull _ = asks taDockerPullExitCode
  dockerRun _ = asks taDockerRunExitCode
  dockerRunStdout _ = asks $ (,"") . taDockerRunExitCode
  dockerImageRm _ = pure ()

instance Example (TestAppT a) where
  type Arg (TestAppT a) = TestApp

  evaluateExample (TestAppT ex) params action =
    evaluateExample
      (action $ \app -> void $ runLoggerLoggingT app $ runReaderT ex app)
      params
      ($ ())

withTestApp :: SpecWith TestApp -> Spec
withTestApp = before loadTestApp

loadTestApp :: IO TestApp
loadTestApp = do
  loadEnvFrom ".env.test"
  TestApp
    <$> newLoggerEnv
    <*> FS.build "/" []
    <*> pure ExitSuccess
    <*> pure ExitSuccess

testAppExample :: TestAppT a -> TestAppT a
testAppExample = id

someRestyler :: String -> Restyler
someRestyler name =
  Restyler
    { rEnabled = True
    , rName = name
    , rImage = "restyled/restyler-" <> name <> ":v1.0.0"
    , rCommand = ["restyle"]
    , rDocumentation = []
    , rArguments = []
    , rInclude = ["**/*"]
    , rInterpreters = []
    , rDelimiters = Nothing
    , rRunStyle = RestylerRunStylePathsOverwriteSep
    }

-- | 'shouldThrow' but in 'MonadUnliftIO' and handling annotations
shouldThrow
  :: (MonadUnliftIO m, Exception e, HasCallStack) => m a -> Selector e -> m ()
action `shouldThrow` p = do
  r <- tryAnnotated action
  case r of
    Right _ ->
      expectationFailure
        $ "did not get expected exception: "
        <> exceptionType
    Left aex@(AnnotatedException {exception}) ->
      case fromException exception of
        Nothing ->
          expectationFailure
            $ "Did not get expected exception type"
            <> "\n  Expected type: "
            <> exceptionType
            <> "\n       Received: "
            <> unpack (displayAnnotatedException aex)
        Just ex ->
          (`expectTrue` p ex)
            $ "predicate failed on expected exception: "
            <> exceptionType
            <> "\n"
            <> show ex
 where
  -- a string representation of the expected exception's type
  exceptionType = (show . typeOf . instanceOf) p
   where
    instanceOf :: Selector a -> a
    instanceOf _ = error "Test.Hspec.Expectations.shouldThrow: broken Typeable instance"

infix 1 `shouldThrow`

expectTrue :: (MonadIO m, HasCallStack) => String -> Bool -> m ()
expectTrue msg b = unless b (expectationFailure msg)
