module Restyler.Restyler.RunSpec
    ( spec
    )
where

import SpecHelper

import Restyler.App.Error
import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.Restyler.Run
import qualified System.Process as Process

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

    describe "runRestyler" $ do
        it "rescues exit code exceptions to RestylerError" $ do
            let errCallProcess _ _ =
                    mapAppError SystemError $ liftIO $ Process.callProcess
                        "false"
                        []
                errMessage = "callProcess: false (exit 1): failed"
                runTestApp = runRIO testApp
                    { taGetCurrentDirectory = pure "/tmp/ignored"
                    , taCallProcess = errCallProcess
                    }

            runTestApp (runRestyler someRestyler ["foo bar"])
                `shouldThrow` isRestylerError someRestyler errMessage

isRestylerError :: Restyler -> String -> AppError -> Bool
isRestylerError r msg (RestylerError re ex) = re == r && show ex == msg
isRestylerError _ _ _ = False
