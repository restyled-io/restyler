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

import Restyler.Prelude

import Blammo.Logging.LogSettings (defaultLogSettings)
import Blammo.Logging.Logger (newTestLogger)
import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.Restyler.Run
import Restyler.Test.App
import Restyler.Test.FS (FS, HasFS (..))
import Restyler.Test.FS qualified as FS

data TestApp = TestApp
  { logger :: Logger
  , fs :: FS
  }

instance HasLogger TestApp where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasFS TestApp where
  fsL = lens (.fs) $ \x y -> x {fs = y}

withTestApp :: SpecWith TestApp -> Spec
withTestApp =
  before
    $ TestApp
    <$> newTestLogger defaultLogSettings
    <*> FS.build "/" []

spec :: Spec
spec = withTestApp $ do
  describe "withFilteredPaths" $ do
    it "does not bring excluded files back by shebang" $ do
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

  describe "findFiles" $ do
    it "expands and excludes" $ do
      writeFile "/foo/bar/baz/bat" ""
      writeFile "/foo/bar/baz/quix" ""
      writeFile "/foo/bat/baz" ""
      writeFile "/foo/foo" ""
      writeFile "/foo/xxx" ""
      setCurrentDirectory "/foo"

      findFiles ["bar/baz", "bat", "xxx", "zzz"]
        `shouldReturn` ["bar/baz/bat", "bar/baz/quix", "bat/baz", "xxx"]

    it "excludes symlinks" $ do
      writeFile "/foo/bar" ""
      createFileLink "/foo/bat" "/foo/bar"

      findFiles ["foo"] `shouldReturn` ["foo/bar"]

    it "doesn't include hidden files" $ do
      writeFile "/foo/bar/baz/bat" ""
      writeFile "/foo/bar/baz/.quix" ""
      writeFile "/foo/bar/baz/.foo/bar" ""
      writeFile "/foo/bat/baz" ""
      writeFile "/foo/foo" ""
      writeFile "/foo/xxx" ""
      setCurrentDirectory "/foo"

      findFiles ["bar/baz", "bat", "xxx", "zzz"]
        `shouldReturn` ["bar/baz/bat", "bat/baz", "xxx"]

    it "includes hidden files given explicitly" $ do
      writeFile "/foo/.bar/baz/bat" ""
      writeFile "/foo/.bar/baz/quix" ""
      writeFile "/foo/bat/baz" ""
      writeFile "/foo/foo" ""
      writeFile "/foo/xxx" ""
      setCurrentDirectory "/foo"

      findFiles [".bar/baz", "bat", "xxx", "zzz"]
        `shouldReturn` [".bar/baz/bat", ".bar/baz/quix", "bat/baz", "xxx"]

someRestyler :: String -> Restyler
someRestyler name =
  Restyler
    { rEnabled = True
    , rName = name
    , rImage = "restyled/restyler-" <> name <> ":v1.0.0"
    , rCommand = ["restyle"]
    , rDocumentation = []
    , rArguments = []
    , rInclude = ["**/*"]
    , rInterpreters = []
    , rDelimiters = Nothing
    , rRunStyle = RestylerRunStylePathsOverwriteSep
    }
