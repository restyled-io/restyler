module Restyler.Restyler.RunSpec
  ( spec
  ) where

import SpecHelper

import Restyler.Config.Interpreter
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

  describe "runRestyler_" $ do
    it "treats non-zero exit codes as RestylerExitFailure"
      $ testAppExample
      $ do
        pendingWith "The separate docker-pull process fails first now"
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
