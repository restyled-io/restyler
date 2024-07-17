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
import Data.Yaml (decodeThrow)
import LoadEnv (loadEnvFrom)
import Restyler.Config
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Options
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

instance HasLogger TestApp where
  loggerL = lens taLogger $ \x y -> x {taLogger = y}

instance HasHostDirectoryOption TestApp where
  getHostDirectoryOption = getHostDirectoryOption . taOptions

instance HasImageCleanupOption TestApp where
  getImageCleanupOption = getImageCleanupOption . taOptions

instance HasRestrictions TestApp where
  getRestrictions = getRestrictions . taOptions

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

instance MonadProcess TestAppT where
  callProcess _cmd _args = pure ()
  callProcessExitCode _cmd _args = asks taProcessExitCodes
  readProcess _cmd _args = pure ""
  readProcessExitCode _cmd _args = pure (ExitSuccess, "")

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
    { oAccessToken = error "oAccessToken"
    , oLogSettings = error "oLogSettings"
    , oOwner = error "oOwner"
    , oRepo = error "oRepo"
    , oPullRequest = error "oPullRequest"
    , oManifest = Nothing
    , oJobUrl = error "oJobUrl"
    , oHostDirectory = Nothing
    , oRepoDisabled = False
    , oPlanRestriction = Nothing
    , oPlanUpgradeUrl = Nothing
    , oRestrictions = fullRestrictions
    , oStatsdHost = Nothing
    , oStatsdPort = Nothing
    , oImageCleanup = False
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
  config <- either throwIO pure $ decodeThrow defaultConfigContent
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

shouldThrow
  :: (HasCallStack, MonadUnliftIO m, Exception e) => m a -> Selector e -> m ()
shouldThrow f matcher = withRunInIO $ \runInIO -> do
  runInIO f `Hspec.shouldThrow` matcher
