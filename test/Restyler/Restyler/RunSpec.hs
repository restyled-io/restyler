module Restyler.Restyler.RunSpec
    ( spec
    ) where

import SpecHelper

import qualified Relude as Prelude
import Restyler.App.Error
import Restyler.Config
import Restyler.Config.ChangedPaths
import Restyler.Config.Interpreter
import Restyler.Options
import Restyler.Restyler
import Restyler.Restyler.Run
import Restyler.Test.FS
    (createFileLink, writeFileExecutable, writeFileUnreadable)

spec :: Spec
spec = withTestApp $ do
    describe "withFilteredPaths" $ do
        it "does not bring excluded files back by shebang" $ testAppExample $ do
            writeFileExecutable "/a" "#!/bin/sh\necho A\n"
            writeFileExecutable "/b" "#!/bin/sh\necho B\n"

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

        it "ignores unreadable (invalid utf-8 byte) files" $ testAppExample $ do
            -- Capture the UTF-8 exception we see on such files
            ex <- liftIO $ handle (pure @IO @IOException) $ do
                void $ Prelude.readFile
                    "test/files/AsanaMathJax_Alphabets-Regular.eot"
                pure $ error "UTF-8 exception expected"

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
            it "has a default maximum" $ testAppExample $ do
                runChangedPaths (mkPaths 1001) id `shouldThrow` \case
                    RestyleError message ->
                        message
                            == "Number of changed paths (1001) is greater than configured maximum (1000)"
                    _ -> False

            it "can be configured" $ testAppExample $ do
                runChangedPaths (mkPaths 11) (setMaximum 10) `shouldThrow` \case
                    RestyleError message ->
                        message
                            == "Number of changed paths (11) is greater than configured maximum (10)"
                    _ -> False

            it "can be configured to skip" $ testAppExample $ do
                runChangedPaths (mkPaths 1001) setOutcomeSkip `shouldReturn` ()

    describe "runRestyler_" $ do
        it "treats non-zero exit codes as RestylerExitFailure"
            $ testAppExample
            $ do
                  pendingWith "Need callProcessExitCode to spoof a 99"

                  runRestyler_ someRestyler ["foo"] `shouldThrow` \case
                      RestylerExitFailure re s _ ->
                          re == someRestyler && s == 99
                      _ -> False

    describe "findFiles" $ do
        it "expands and excludes" $ testAppExample $ do
            writeFile "/foo/bar/baz/bat" ""
            writeFile "/foo/bar/baz/quix" ""
            writeFile "/foo/bat/baz" ""
            writeFile "/foo/foo" ""
            writeFile "/foo/xxx" ""
            setCurrentDirectory "/foo"

            findFiles ["bar/baz", "bat", "xxx", "zzz"]
                `shouldReturn` ["bar/baz/bat", "bar/baz/quix", "bat/baz", "xxx"]

        it "excludes symlinks" $ testAppExample $ do
            writeFile "/foo/bar" ""
            createFileLink "/foo/bar" "/foo/baz/bat"

            findFiles ["foo"] `shouldReturn` ["foo/bar"]

mkPaths :: Int -> [FilePath]
mkPaths n = map (\i -> "/" <> show i <> ".txt") [1 .. n]

runChangedPaths
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadSystem m
       , MonadProcess m
       , MonadDownloadFile m
       , MonadReader env m
       , HasOptions env
       )
    => [FilePath]
    -> (ChangedPathsConfig -> ChangedPathsConfig)
    -> m ()
runChangedPaths paths f = do
    for_ paths $ \path -> writeFile path ""
    config <- loadDefaultConfig
    let updatedConfig = config { cChangedPaths = f $ cChangedPaths config }
    runRestylers_ updatedConfig paths

setMaximum :: Natural -> ChangedPathsConfig -> ChangedPathsConfig
setMaximum m cp = cp { cpcMaximum = m }

setOutcomeSkip :: ChangedPathsConfig -> ChangedPathsConfig
setOutcomeSkip cp = cp { cpcOutcome = MaximumChangedPathsOutcomeSkip }
