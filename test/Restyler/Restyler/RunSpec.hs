module Restyler.Restyler.RunSpec
  ( spec
  ) where

import SpecHelper

import Restyler.Config
import Restyler.Config.ChangedPaths
import Restyler.Config.Interpreter
import Restyler.Options.HostDirectory
import Restyler.Options.ImageCleanup
import Restyler.Restrictions
import Restyler.Restyler
import Restyler.Restyler.Run
import Restyler.Test.FS (createFileLink, writeFileExecutable)

spec :: Spec
spec = withTestApp $ do
  describe "withFilteredPaths" $ do
    it "does not bring excluded files back by shebang" $ testAppExample $ do
      writeFileExecutable "/a" "#!/bin/sh\necho A\n"
      writeFileExecutable "/b" "#!/bin/sh\necho B\n"

      filtered <-
        withFilteredPaths
          [ (someRestyler "") {rInclude = ["**/*.sh"], rInterpreters = [Sh]}
          , (someRestyler "")
              { rInclude = ["**/*.sh", "!b"]
              , rInterpreters = [Sh]
              }
          ]
          ["a", "b"]
          (const pure)

      filtered `shouldBe` [["a", "b"], ["a"]]

  describe "runRestylers_" $ do
    context "maximum changed paths" $ do
      it "has a default maximum" $ testAppExample $ do
        runChangedPaths (mkPaths 1001) id
          `shouldThrow` (== TooManyChangedPaths 1001 1000)

      it "can be configured" $ testAppExample $ do
        runChangedPaths (mkPaths 11) (setMaximum 10)
          `shouldThrow` (== TooManyChangedPaths 11 10)

      it "can be configured to skip" $ testAppExample $ do
        runChangedPaths (mkPaths 1001) setOutcomeSkip `shouldReturn` ()

  describe "runRestyler_" $ do
    it "treats non-zero exit codes as RestylerExitFailure"
      $ testAppExample
      $ do
        local (\x -> x {taProcessExitCodes = ExitFailure 99}) $ do
          runRestyler_ (someRestyler "foo") ["bar"]
            `shouldThrow` ( ==
                              RestylerExitFailure
                                (someRestyler "foo")
                                99
                          )

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
     , HasLogger env
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasRestrictions env
     )
  => [FilePath]
  -> (ChangedPathsConfig -> ChangedPathsConfig)
  -> m ()
runChangedPaths paths f = do
  for_ paths $ \path -> writeFile path ""
  config <- loadDefaultConfig
  let updatedConfig = config {cChangedPaths = f $ cChangedPaths config}
  runRestylers_ updatedConfig paths

setMaximum :: Natural -> ChangedPathsConfig -> ChangedPathsConfig
setMaximum m cp = cp {maximum = m}

setOutcomeSkip :: ChangedPathsConfig -> ChangedPathsConfig
setOutcomeSkip cp = cp {outcome = MaximumChangedPathsOutcomeSkip}
