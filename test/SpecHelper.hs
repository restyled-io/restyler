{-# LANGUAGE FieldSelectors #-}

module SpecHelper
  ( someRestyler

    -- * TestApp
  , TestApp (..)
  , TestAppT (..)
  , withTestApp
  , testAppExample

    -- * Config
  , loadDefaultConfig
  , testRestylers

    -- * Re-exports
  , module X
  , pendingWith
  , shouldThrow
  ) where

import Restyler.App.Class as X
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
import Data.Yaml (decodeThrow)
import LoadEnv (loadEnvFrom)
import Restyler.AnnotatedException
import Restyler.Config
import Restyler.Docker
import Restyler.Git
import Restyler.Local.Options
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Restrictions
import Restyler.Restyler
import Restyler.Test.FS (FS, HasFS (..))
import Restyler.Test.FS qualified as FS
import Test.Hspec qualified as Hspec
import Test.Hspec.Core.Spec (Example (..))

data TestApp = TestApp
  { taLogger :: Logger
  , taOptions :: Options
  , taFS :: FS
  , taProcessExitCodes :: ExitCode
  }
  deriving (HasHostDirectoryOption, HasRestrictions) via (ThroughOptions TestApp)
  deriving (HasImageCleanupOption) via (NoImageCleanupOption TestApp)

instance HasLogger TestApp where
  loggerL = lens taLogger $ \x y -> x {taLogger = y}

instance HasOptions TestApp where
  getOptions = taOptions

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
  deriving (MonadGit) via (NullGit TestAppT)
  deriving (MonadDocker) via (NullDocker TestAppT)

instance MonadSystem TestAppT where
  getCurrentDirectory = FS.getCurrentDirectory
  setCurrentDirectory = FS.setCurrentDirectory
  doesFileExist = FS.doesFileExist
  doesDirectoryExist = FS.doesDirectoryExist
  isFileExecutable = FS.isFileExecutable
  isFileSymbolicLink = FS.isFileSymbolicLink
  listDirectory = FS.listDirectory
  readFileBS = FS.readFileBinary
  writeFile = FS.writeFileUtf8
  removeFile = FS.removeFile

instance MonadDownloadFile TestAppT where
  downloadFile _url _path = pure ()

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
    <*> pure testOptions
    <*> FS.build "/" []
    <*> pure ExitSuccess

testOptions :: Options
testOptions =
  Options
    { logSettings = error "logSettings"
    , restrictions = fullRestrictions
    , hostDirectory = toHostDirectoryOption Nothing
    }

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

loadDefaultConfig :: MonadIO m => m Config
loadDefaultConfig = do
  config <- either throw pure $ decodeThrow defaultConfigContent
  resolveRestylers config testRestylers

testRestylers :: [Restyler]
testRestylers =
  [ someRestyler "astyle"
  , someRestyler "autopep8"
  , someRestyler "black"
  , someRestyler "dfmt"
  , someRestyler "elm-format"
  , (someRestyler "hindent") {rEnabled = False}
  , (someRestyler "jdt") {rEnabled = False}
  , someRestyler "pg_format"
  , someRestyler "php-cs-fixer"
  , someRestyler "prettier"
  , someRestyler "prettier-markdown"
  , someRestyler "prettier-ruby"
  , someRestyler "prettier-yaml"
  , someRestyler "reorder-python-imports"
  , someRestyler "rubocop"
  , someRestyler "rustfmt"
  , someRestyler "shellharden"
  , someRestyler "shfmt"
  , someRestyler "stylish-haskell"
  , someRestyler "terraform"
  , someRestyler "yapf"
  ]

pendingWith :: (HasCallStack, MonadIO m) => String -> m ()
pendingWith = liftIO . Hspec.pendingWith

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
