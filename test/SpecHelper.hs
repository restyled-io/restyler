module SpecHelper
    ( someRestyler

    -- * TestApp
    , TestApp(..)
    , TestAppT(..)
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
import Restyler.Options
import Restyler.Restyler
import qualified Restyler.Test.FS as FS
import Restyler.Test.FS (FS, HasFS(..))
import qualified Test.Hspec as Hspec
import Test.Hspec.Core.Spec (Example(..))

data TestApp = TestApp
    { taLogger :: Logger
    , taOptions :: Options
    , taFS :: FS
    , taProcessExitCodes :: ExitCode
    }

instance HasLogger TestApp where
    loggerL = lens taLogger $ \x y -> x { taLogger = y }

instance HasOptions TestApp where
    optionsL = lens taOptions $ \x y -> x { taOptions = y }

instance HasFS TestApp where
    fsL = lens taFS $ \x y -> x { taFS = y }

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

instance MonadProcess TestAppT where
    callProcess _cmd _args = pure ()
    callProcessExitCode _cmd _args = asks taProcessExitCodes
    readProcess _cmd _args _stdin = pure ""

instance MonadDownloadFile TestAppT where
    downloadFile _url _path = pure ()

instance Example (TestAppT a) where
    type Arg (TestAppT a) = TestApp

    evaluateExample (TestAppT ex) params action = evaluateExample
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
testOptions = Options
    { oAccessToken = error "oAccessToken"
    , oLogSettings = error "oLogSettings"
    , oOwner = error "oOwner"
    , oRepo = error "oRepo"
    , oPullRequest = error "oPullRequest"
    , oJobUrl = error "oJobUrl"
    , oHostDirectory = Nothing
    , oUnrestricted = False
    , oStatsdHost = Nothing
    , oStatsdPort = Nothing
    }

testAppExample :: TestAppT a -> TestAppT a
testAppExample = id

someRestyler :: Restyler
someRestyler = Restyler
    { rEnabled = True
    , rName = "test-restyler"
    , rImage = "restyled/restyler-test-restyler"
    , rCommand = ["restyle"]
    , rDocumentation = []
    , rArguments = []
    , rInclude = ["**/*"]
    , rInterpreters = []
    , rDelimiters = Nothing
    , rSupportsArgSep = True
    , rSupportsMultiplePaths = True
    }

loadDefaultConfig :: MonadIO m => m Config
loadDefaultConfig = do
    config <- either throwIO pure $ decodeThrow defaultConfigContent
    resolveRestylers config testRestylers

testRestylers :: [Restyler]
testRestylers =
    [ someRestyler { rName = "astyle" }
    , someRestyler { rName = "autopep8" }
    , someRestyler { rName = "black" }
    , someRestyler { rName = "dfmt" }
    , someRestyler { rName = "elm-format" }
    , someRestyler { rName = "hindent", rEnabled = False }
    , someRestyler { rName = "jdt", rEnabled = False }
    , someRestyler { rName = "pg_format" }
    , someRestyler { rName = "php-cs-fixer" }
    , someRestyler { rName = "prettier" }
    , someRestyler { rName = "prettier-markdown" }
    , someRestyler { rName = "prettier-ruby" }
    , someRestyler { rName = "prettier-yaml" }
    , someRestyler { rName = "reorder-python-imports" }
    , someRestyler { rName = "rubocop" }
    , someRestyler { rName = "rustfmt" }
    , someRestyler { rName = "shellharden" }
    , someRestyler { rName = "shfmt" }
    , someRestyler { rName = "stylish-haskell" }
    , someRestyler { rName = "terraform" }
    , someRestyler { rName = "yapf" }
    ]

pendingWith :: (HasCallStack, MonadIO m) => String -> m ()
pendingWith = liftIO . Hspec.pendingWith

shouldThrow
    :: (HasCallStack, MonadUnliftIO m, Exception e) => m a -> Selector e -> m ()
shouldThrow f matcher = withRunInIO $ \runInIO -> do
    runInIO f `Hspec.shouldThrow` matcher
