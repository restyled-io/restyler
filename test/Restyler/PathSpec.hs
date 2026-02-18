{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- Module      : Restyler.PathSpec
-- Copyright   : (c) 2026 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.PathSpec
  ( spec
  ) where

import Restyler.Prelude

import Restyler.Path
import Restyler.Test.App

spec :: Spec
spec = withSimpleTestApp $ do
  describe "expandSomePaths" $ do
    it "expands a directory" $ do
      writeFile [absfile|/foo/bar/baz/bat|] ""
      writeFile [absfile|/foo/bar/baz/quix|] ""
      writeFile [absfile|/foo/bat/baz|] ""
      setCurrentDirectory [absdir|/foo|]

      expandSomePaths ["bar"]
        `shouldReturn` [ [relfile|bar/baz/bat|]
                       , [relfile|bar/baz/quix|]
                       ]

    it "returns files as-is" $ do
      writeFile [absfile|/foo/bat/baz|] ""
      writeFile [absfile|/foo/foo|] ""
      writeFile [absfile|/foo/xxx|] ""
      setCurrentDirectory [absdir|/foo|]

      expandSomePaths ["foo", "bat/baz"]
        `shouldReturn` [ [relfile|foo|]
                       , [relfile|bat/baz|]
                       ]

    it "expands and excludes" $ do
      writeFile [absfile|/foo/bar/baz/bat|] ""
      writeFile [absfile|/foo/bar/baz/quix|] ""
      writeFile [absfile|/foo/bat/baz|] ""
      writeFile [absfile|/foo/foo|] ""
      writeFile [absfile|/foo/xxx|] ""
      setCurrentDirectory [absdir|/foo|]

      expandSomePaths ["bar/baz", "bat", "xxx", "zzz"]
        `shouldReturn` [ [relfile|bar/baz/bat|]
                       , [relfile|bar/baz/quix|]
                       , [relfile|bat/baz|]
                       , [relfile|xxx|]
                       ]

    it "excludes symlinks given" $ do
      writeFile [absfile|/foo/bar|] ""
      createFileLink [absfile|/foo/bar|] [absfile|/foo/bat|]

      expandSomePaths ["foo/bar"] `shouldReturn` [[relfile|foo/bar|]]
      expandSomePaths ["foo/bat"] `shouldReturn` []

    it "excludes symlinks within given directory" $ do
      writeFile [absfile|/foo/bar|] ""
      createFileLink [absfile|/foo/bar|] [absfile|/foo/bat|]

      expandSomePaths ["foo"] `shouldReturn` [[relfile|foo/bar|]]

    it "doesn't include hidden files" $ do
      writeFile [absfile|/foo/bar/baz/bat|] ""
      writeFile [absfile|/foo/bar/baz/.quix|] ""
      writeFile [absfile|/foo/bar/baz/.foo/bar|] ""
      writeFile [absfile|/foo/bat/baz|] ""
      writeFile [absfile|/foo/foo|] ""
      writeFile [absfile|/foo/xxx|] ""
      setCurrentDirectory [absdir|/foo|]

      expandSomePaths ["bar/baz", "bat", "xxx", "zzz"]
        `shouldReturn` [ [relfile|bar/baz/bat|]
                       , [relfile|bat/baz|]
                       , [relfile|xxx|]
                       ]

    it "includes hidden files given explicitly" $ do
      writeFile [absfile|/foo/.bar/baz/bat|] ""
      writeFile [absfile|/foo/.bar/baz/quix|] ""
      writeFile [absfile|/foo/bat/baz|] ""
      writeFile [absfile|/foo/foo|] ""
      writeFile [absfile|/foo/xxx|] ""
      setCurrentDirectory [absdir|/foo|]

      expandSomePaths [".bar/baz", "bat", "xxx", "zzz"]
        `shouldReturn` [ [relfile|.bar/baz/bat|]
                       , [relfile|.bar/baz/quix|]
                       , [relfile|bat/baz|]
                       , [relfile|xxx|]
                       ]
