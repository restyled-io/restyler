-- |
--
-- Module      : Restyler.Restyler.RunSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Restyler.RunSpec
  ( spec
  ) where

import SpecHelper

import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.Restyler.Run

spec :: Spec
spec = withTestApp $ do
  describe "withFilteredPaths" $ do
    it "does not bring excluded files back by shebang" $ testAppExample $ do
      writeFile "/a" "#!/bin/sh\necho A\n"
      modifyPermissions "/a" $ \p -> p {executable = True}
      writeFile "/b" "#!/bin/sh\necho B\n"
      modifyPermissions "/b" $ \p -> p {executable = True}

      filtered <-
        withFilteredPaths
          [ (someRestyler "") {rInclude = ["**/*.sh"], rInterpreters = [Sh]}
          , (someRestyler "")
              { rInclude = ["**/*.sh", "!b"]
              , rInterpreters = [Sh]
              }
          ]
          ["a", "b"]
          (const $ pure . Just)

      filtered `shouldBe` Just (["a", "b"] :| [["a"]])

  describe "runRestyler" $ do
    it "treats non-zero exit codes as RestylerExitFailure" $ testAppExample $ do
      local (\x -> x {taDockerRunExitCode = ExitFailure 99}) $ do
        runRestyler (someRestyler "foo") ["bar"]
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
      createFileLink "/foo/bat" "/foo/bar"

      findFiles ["foo"] `shouldReturn` ["foo/bar"]

    it "doesn't include hidden files" $ testAppExample $ do
      writeFile "/foo/bar/baz/bat" ""
      writeFile "/foo/bar/baz/.quix" ""
      writeFile "/foo/bar/baz/.foo/bar" ""
      writeFile "/foo/bat/baz" ""
      writeFile "/foo/foo" ""
      writeFile "/foo/xxx" ""
      setCurrentDirectory "/foo"

      findFiles ["bar/baz", "bat", "xxx", "zzz"]
        `shouldReturn` ["bar/baz/bat", "bat/baz", "xxx"]

    it "includes hidden files given explicitly" $ testAppExample $ do
      writeFile "/foo/.bar/baz/bat" ""
      writeFile "/foo/.bar/baz/quix" ""
      writeFile "/foo/bat/baz" ""
      writeFile "/foo/foo" ""
      writeFile "/foo/xxx" ""
      setCurrentDirectory "/foo"

      findFiles [".bar/baz", "bat", "xxx", "zzz"]
        `shouldReturn` [".bar/baz/bat", ".bar/baz/quix", "bat/baz", "xxx"]
