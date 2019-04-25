module Restyler.Restyler.RunSpec
    ( spec
    )
where

import SpecHelper

import Restyler.App.Error
import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.Restyler.Run

spec :: Spec
spec = do
    describe "filterRestylePaths" $ do
        it "ignores unreadable (invalid utf-8 byte) files" $ do
            let errReadFile = mapAppError SystemError . readFileUtf8
                runTestApp = runRIO testApp { taReadFile = errReadFile }
                restyler = someRestyler
                    { rInclude = ["!**/*.eot"]
                    , rInterpreters = [Ruby]
                    }
                invalidFile = "test/files/AsanaMathJax_Alphabets-Regular.eot"

            filtered <- runTestApp $ do
                filterRestylePaths restyler [invalidFile]

            filtered `shouldBe` []
