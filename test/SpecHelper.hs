module SpecHelper
    ( TestApp(..)
    , testApp
    , someRestyler

    -- * Re-exports
    , module X
    )
where

import Restyler.Prelude as X
import Test.Hspec as X
import Test.QuickCheck as X

import Restyler.App.Class
import Restyler.Options
import Restyler.Restyler

-- | A versatile app for use with @'runRIO'@
--
-- Be sure to construct valid actions for the fields exercised in your test. The
-- initialization function (@'testApp'@) sets them all to @'error'@ values.
--
data TestApp = TestApp
    { taLogFunc :: LogFunc
    , taOptions :: Options

    -- System
    , taReadFile :: FilePath -> RIO TestApp Text
    , taReadFileBS :: FilePath -> RIO TestApp ByteString
    , taGetCurrentDirectory :: RIO TestApp FilePath
    , taSetCurrentDirectory :: FilePath -> RIO TestApp ()
    , taDoesFileExist :: FilePath -> RIO TestApp Bool

    -- Process
    , taCallProcess :: String -> [String] -> RIO TestApp ()
    , taCallProcessExitCode :: String -> [String] -> RIO TestApp ExitCode
    , taReadProcess :: String -> [String] -> String -> RIO TestApp String

    -- Add our other capabilities if/when tests require them
    }

testApp :: TestApp
testApp = TestApp
    { taLogFunc = mkLogFunc $ \_ _ _ _ -> pure ()
    , taOptions = testOptions
    , taReadFile = error "readFile"
    , taReadFileBS = error "readFileBS"
    , taGetCurrentDirectory = error "getCurrentDirectory"
    , taSetCurrentDirectory = error "setCurrentDirectory"
    , taDoesFileExist = error "doesFileExist"
    , taCallProcess = error "callProcess"
    , taCallProcessExitCode = error "callProcessExitCode"
    , taReadProcess = error "readProcess"
    }

testOptions :: Options
testOptions = Options
    { oAccessToken = error "oAccessToken"
    , oLogLevel = error "oLogLevel"
    , oLogColor = error "oLogColor"
    , oOwner = error "oOwner"
    , oRepo = error "oRepo"
    , oPullRequest = error "oPullRequest"
    , oJobUrl = error "oJobUrl"
    , oHostDirectory = Nothing
    , oUnrestricted = False
    }

instance HasLogFunc TestApp where
    logFuncL = lens taLogFunc $ \x y -> x { taLogFunc = y }

instance HasOptions TestApp where
    optionsL = lens taOptions $ \x y -> x { taOptions = y }

instance HasSystem TestApp where
    readFile = asksAp1 taReadFile
    readFileBS = asksAp1 taReadFileBS
    getCurrentDirectory = asksAp taGetCurrentDirectory
    setCurrentDirectory = asksAp1 taSetCurrentDirectory
    doesFileExist = asksAp1 taDoesFileExist

instance HasProcess TestApp where
    callProcess = asksAp2 taCallProcess
    callProcessExitCode = asksAp2 taCallProcessExitCode
    readProcess = asksAp3 taReadProcess

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
    , rSupportsArgSep = True
    , rSupportsMultiplePaths = True
    }

-- | @'asks'@ a function off the environment and apply it
asksAp :: MonadReader r m => (r -> m a) -> m a
asksAp f = join $ asks f

-- | Same, but apply it to 1 argument
asksAp1 :: MonadReader r m => (r -> a -> m b) -> a -> m b
asksAp1 f x = do
    f' <- asks f
    f' x

-- | Same, but apply it to 2 arguments
asksAp2 :: MonadReader r m => (r -> a -> b -> m c) -> a -> b -> m c
asksAp2 f x y = do
    f' <- asks f
    f' x y

-- | Same, but apply it to 3 arguments
asksAp3 :: MonadReader r m => (r -> a -> b -> c -> m d) -> a -> b -> c -> m d
asksAp3 f x y z = do
    f' <- asks f
    f' x y z
