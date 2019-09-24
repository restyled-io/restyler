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

    describe "runRestyler_" $ do
        it "treats non-zero exit codes as RestylerExitFailure" $ do
            let
                runTestApp = runRIO testApp
                    { taGetCurrentDirectory = pure "/tmp/ignored"
                    , taCallProcessExitCode = \_ _ -> pure $ ExitFailure 99
                    }

            runTestApp (runRestyler_ someRestyler ["foo bar"])
                `shouldThrow` isRestylerExitFailure someRestyler 99

isRestylerExitFailure :: Restyler -> Int -> AppError -> Bool
isRestylerExitFailure r se (RestylerExitFailure re s _) = re == r && se == s
isRestylerExitFailure _ _ _ = False
