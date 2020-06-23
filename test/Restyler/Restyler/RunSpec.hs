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
import qualified RIO
import RIO.Test.FS (writeFileExecutable, writeFileUnreadable)

spec :: Spec
spec = do
    describe "withFilteredPaths" $ do
        it "does not bring excluded files back by shebang" $ do
            filtered <- runTestApp $ do
                writeFileExecutable "/a" "#!/bin/sh\necho A\n"
                writeFileExecutable "/b" "#!/bin/sh\necho B\n"

                withFilteredPaths
                    [ someRestyler
                        { rInclude = ["**/*.sh"]
                        , rInterpreters = [Sh]
                        }
                    , someRestyler
                        { rInclude = ["**/*.sh", "!b"]
                        , rInterpreters = [Sh]
                        }
                    ]
                    ["a", "b"]
                    (const pure)

            filtered `shouldBe` [["a", "b"], ["a"]]

        it "ignores unreadable (invalid utf-8 byte) files" $ do
            -- Capture the UTF-8 exception we see on such files
            ex <- handle (pure @IO @IOException) $ do
                void $ RIO.readFileUtf8
                    "test/files/AsanaMathJax_Alphabets-Regular.eot"
                pure $ error "UTF-8 exception expected"

            runTestApp $ do
                writeFileUnreadable "invalid.eot" ex

                filtered <- withFilteredPaths
                    [ someRestyler
                          { rInclude = ["!**/*.eot"]
                          , rInterpreters = [Ruby]
                          }
                    ]
                    ["invalid.eot"]
                    (const pure)

                liftIO $ filtered `shouldBe` [[]]

    describe "runRestyler_" $ do
        it "treats non-zero exit codes as RestylerExitFailure" $ do
            let
                runTestApp' f = do
                    app <- testApp "/" []
                    runRIO
                        app
                            { taCallProcessExitCode = \_ _ ->
                                pure $ ExitFailure 99
                            }
                        f

            runTestApp' (runRestyler_ someRestyler ["foo bar"])
                `shouldThrow` isRestylerExitFailure someRestyler 99

isRestylerExitFailure :: Restyler -> Int -> AppError -> Bool
isRestylerExitFailure r se (RestylerExitFailure re s _) = re == r && se == s
isRestylerExitFailure _ _ _ = False
