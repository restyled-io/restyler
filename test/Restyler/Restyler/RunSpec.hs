{-# LANGUAGE TypeApplications #-}

module Restyler.Restyler.RunSpec
    ( spec
    )
where

import SpecHelper

import Restyler.App.Error
import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.Restyler.Run
import RIO.Test.FS (writeFileUnreadable)

spec :: Spec
spec = do
    describe "filterRestylePaths" $ do
        it "ignores unreadable (invalid utf-8 byte) files" $ do
            -- Capture the UTF-8 exception we see on such files
            ex <- handle (pure @IO @IOException) $ do
                void $ readFileUtf8
                    "test/files/AsanaMathJax_Alphabets-Regular.eot"
                pure $ error "UTF-8 exception expected"

            app <- testApp "/" [("/invalid.eot", "")]
            runRIO app $ do
                writeFileUnreadable "invalid.eot" ex

                filtered <- filterRestylePaths
                    someRestyler
                        { rInclude = ["!**/*.eot"]
                        , rInterpreters = [Ruby]
                        }
                    ["invalid.eot"]

                liftIO $ filtered `shouldBe` []

        describe "runRestyler_" $ do
            it "treats non-zero exit codes as RestylerExitFailure" $ do
                app <- testApp "/" []

                let
                    runTestApp = runRIO app
                        { taCallProcessExitCode = \_ _ -> pure $ ExitFailure 99
                        }

                runTestApp (runRestyler_ someRestyler ["foo bar"])
                    `shouldThrow` isRestylerExitFailure someRestyler 99

isRestylerExitFailure :: Restyler -> Int -> AppError -> Bool
isRestylerExitFailure r se (RestylerExitFailure re s _) = re == r && se == s
isRestylerExitFailure _ _ _ = False
