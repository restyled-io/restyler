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
import Restyler.Restyler

-- | A versatile app for use with @'runRIO'@
--
-- Be sure to construct valid actions for the fields exercised in your test. The
-- initialization function (@'testApp'@) sets them all to @'error'@ values.
--
data TestApp = TestApp
    { taLogFunc :: LogFunc

    -- System
    , taReadFile :: FilePath -> RIO TestApp Text
    , taGetCurrentDirectory :: RIO TestApp FilePath
    , taSetCurrentDirectory :: FilePath -> RIO TestApp ()
    , taDoesFileExist :: FilePath -> RIO TestApp Bool

    -- Process
    , taCallProcess :: String -> [String] -> RIO TestApp ()
    , taReadProcess :: String -> [String] -> String -> RIO TestApp String

    -- Add our other capabilities if/when tests require them
    }

testApp :: TestApp
testApp = TestApp
    { taLogFunc = mkLogFunc $ \_ _ _ _ -> pure ()
    , taReadFile = error "readFile"
    , taGetCurrentDirectory = error "getCurrentDirectory"
    , taSetCurrentDirectory = error "setCurrentDirectory"
    , taDoesFileExist = error "doesFileExist"
    , taCallProcess = error "callProcess"
    , taReadProcess = error "readProcess"
    }

instance HasLogFunc TestApp where
    logFuncL = lens taLogFunc $ \x y -> x { taLogFunc = y }

instance HasSystem TestApp where
    readFile = asksAp1 taReadFile
    getCurrentDirectory = asksAp taGetCurrentDirectory
    setCurrentDirectory = asksAp1 taSetCurrentDirectory
    doesFileExist = asksAp1 taDoesFileExist

instance HasProcess TestApp where
    callProcess = asksAp2 taCallProcess
    readProcess = asksAp3 taReadProcess

someRestyler :: Restyler
someRestyler = Restyler
    { rName = "test-restyler"
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
