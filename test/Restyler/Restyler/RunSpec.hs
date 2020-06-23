{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Restyler.Restyler.RunSpec
    ( spec
    )
where

import SpecHelper

import Restyler.App.Error
import Restyler.Config
import Restyler.Config.ChangedPaths
import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.Restyler.Run
import qualified RIO
import RIO.Test.FS (writeFileExecutable, writeFileUnreadable, writeFileUtf8)

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

    describe "runRestylers_" $ do
        context "maximum changed paths" $ do
            it "has a default maximum" $ do
                runChangedPaths (mkPaths 1001) id `shouldThrow` \case
                    RestyleError message ->
                        message
                            == "Number of changed paths (1001) is greater than configured maxium (1000)"
                    _ -> False

            it "can be configured" $ do
                runChangedPaths (mkPaths 11) (setMaximum 10) `shouldThrow` \case
                    RestyleError message ->
                        message
                            == "Number of changed paths (11) is greater than configured maxium (10)"
                    _ -> False

            it "can be configured to skip" $ do
                runChangedPaths (mkPaths 1001) setOutcomeSkip `shouldReturn` ()

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

mkPaths :: Int -> [FilePath]
mkPaths n = map (\i -> "/" <> show i <> ".txt") [1 .. n]

runChangedPaths
    :: [FilePath] -> (ChangedPathsConfig -> ChangedPathsConfig) -> IO ()
runChangedPaths paths f = runTestApp $ do
    for_ paths $ \path -> writeFileUtf8 path ""
    config <- loadDefaultConfig
    let updatedConfig = config { cChangedPaths = f $ cChangedPaths config }
    runRestylers_ updatedConfig paths

setMaximum :: Natural -> ChangedPathsConfig -> ChangedPathsConfig
setMaximum m cp = cp { cpcMaximum = m }

setOutcomeSkip :: ChangedPathsConfig -> ChangedPathsConfig
setOutcomeSkip cp = cp { cpcOutcome = MaximumChangedPathsOutcomeSkip }

isRestylerExitFailure :: Restyler -> Int -> AppError -> Bool
isRestylerExitFailure r se (RestylerExitFailure re s _) = re == r && se == s
isRestylerExitFailure _ _ _ = False
