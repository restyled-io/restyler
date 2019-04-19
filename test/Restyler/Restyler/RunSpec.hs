module Restyler.Restyler.RunSpec
    ( spec
    )
where

import SpecHelper

import Restyler.App.Class
import Restyler.App.Error
import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.Restyler.Run

data TestApp = TestApp

instance HasSystem TestApp where
    -- Must throw AppError to match real implementation
    readFile = handleIO (throwIO . SystemError) . readFileUtf8

    -- Not used in this test
    getCurrentDirectory = error "getCurrentDirectory"
    setCurrentDirectory = error "setCurrentDirectory"
    doesFileExist = error "doesFileExist"

runTestApp :: RIO TestApp a -> IO a
runTestApp = runRIO TestApp

spec :: Spec
spec = do
    describe "filterRestylePaths" $ do
        it "ignores unreadable (invalid utf-8 byte) files" $ do
            let invalidFile = "test/files/AsanaMathJax_Alphabets-Regular.eot"
                someRestyler = Restyler
                    { rName = "test-restyler"
                    , rImage = "restyled/restyler-test-restyler"
                    , rCommand = ["restyle"]
                    , rArguments = []
                    , rInclude = ["!**/*.eot"]
                    , rInterpreters = [Ruby]
                    , rSupportsArgSep = True
                    , rSupportsMultiplePaths = True
                    }

            filtered <- runTestApp $ do
                filterRestylePaths someRestyler [invalidFile]

            filtered `shouldBe` []
