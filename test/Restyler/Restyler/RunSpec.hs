module Restyler.Restyler.RunSpec
    ( spec
    )
where

import SpecHelper

import qualified RIO
import Restyler.App.Error
import Restyler.Capabilities.Process.Mock
import Restyler.Capabilities.System
import Restyler.Capabilities.System.State
import Restyler.Config
import Restyler.Config.ChangedPaths
import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.Restyler.Run
import Restyler.TestApp

spec :: Spec
spec = do
    describe "withFilteredPaths" $ do
        it "does not bring excluded files back by shebang" $ runTestApp $ do
            addExecutableFile "/a" "#!/bin/sh\necho A\n"
            addExecutableFile "/b" "#!/bin/sh\necho B\n"

            filtered <- withFilteredPaths
                [ someRestyler { rInclude = ["**/*.sh"], rInterpreters = [Sh] }
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
                addUnreadableFile "invalid.eot" ex

                filtered <- withFilteredPaths
                    [ someRestyler
                          { rInclude = ["!**/*.eot"]
                          , rInterpreters = [Ruby]
                          }
                    ]
                    ["invalid.eot"]
                    (const pure)

                filtered `shouldBe` [[]]

    describe "runRestylers_" $ do
        context "maximum changed paths" $ do
            it "has a default maximum" $ runTestApp $ do
                config <- loadDefaultConfig
                let paths = mkPaths 1001
                traverse_ (`writeFile` "") paths

                result <- tryError $ runRestylers_ config paths

                result `shouldSatisfy` \case
                    Left (RestyleError message) ->
                        message
                            == "Number of changed paths (1001) is greater than configured maximum (1000)"
                    _ -> False

            it "can be configured" $ runTestApp $ do
                config <- loadDefaultConfig
                let paths = mkPaths 11
                    updatedConfig = config
                        { cChangedPaths = (cChangedPaths config)
                            { cpcMaximum = 10
                            }
                        }
                traverse_ (`writeFile` "") paths

                result <- tryError $ runRestylers_ updatedConfig paths

                result `shouldSatisfy` \case
                    Left (RestyleError message) ->
                        message
                            == "Number of changed paths (11) is greater than configured maximum (10)"
                    _ -> False

            it "can be configured to skip" $ runTestApp $ do
                config <- loadDefaultConfig
                let paths = mkPaths 1001
                    updatedConfig = config
                        { cChangedPaths = (cChangedPaths config)
                            { cpcOutcome = MaximumChangedPathsOutcomeSkip
                            }
                        }
                traverse_ (`writeFile` "") paths

                runRestylers_ updatedConfig paths `shouldReturn` ()

    describe "runRestyler_" $ do
        it "treats non-zero exit codes as RestylerExitFailure" $ runTestApp $ do
            prependProcessMock
                "docker"
                (const True)
                (const True)
                (Right (ExitFailure 42, ""))

            result <- tryError $ runRestyler_ someRestyler ["foo"]

            result `shouldSatisfy` \case
                Left (RestylerExitFailure re s _) ->
                    re == someRestyler && s == 42
                _ -> False

        describe "findFiles" $ do
            it "expands and excludes" $ runTestApp $ do
                traverse_
                    (uncurry writeFile)
                    [ ("/foo/bar/baz/bat", "")
                    , ("/foo/bar/baz/quix", "")
                    , ("/foo/bat/baz", "")
                    , ("/foo/foo", "")
                    , ("/foo/xxx", "")
                    ]
                setCurrentDirectory "/foo"

                findFiles ["bar/baz", "bat", "xxx", "zzz"]
                    `shouldReturn` [ "bar/baz/bat"
                                   , "bar/baz/quix"
                                   , "bat/baz"
                                   , "xxx"
                                   ]

            it "excludes symlinks" $ runTestApp $ do
                writeFile "/foo/bar" ""
                addSymlink "/foo/bar" "/foo/baz/bat"

                findFiles ["foo"] `shouldReturn` ["foo/bar"]

mkPaths :: Int -> [FilePath]
mkPaths n = map (\i -> "/" <> show i <> ".txt") [1 .. n]
