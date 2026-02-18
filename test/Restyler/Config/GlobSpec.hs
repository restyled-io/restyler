{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- Module      : Restyler.Config.GlobSpec
-- Copyright   : (c) 2026 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.GlobSpec
  ( spec
  ) where

import Restyler.Prelude

import Path (relfile)
import Restyler.Config.Glob
import Restyler.Test.App

spec :: Spec
spec = withSimpleTestApp $ do
  describe "matchAnyInDirectory" $ do
    let
      globs :: [Glob FilePath]
      globs =
        [ ".fourmolu.yaml"
        , ".fourmolu/*.yaml"
        , "config/fourmolu/**/*.yaml"
        ]

    it "no matches" $ do
      matchAnyInCurrentDirectory globs `shouldReturn` False

    it "top-level" $ do
      writeFile [relfile|.fourmolu.yaml|] ""
      matchAnyInCurrentDirectory globs `shouldReturn` True

    it "sub-directory" $ do
      writeFile [relfile|.fourmolu/config.yaml|] ""
      matchAnyInCurrentDirectory globs `shouldReturn` True

    it "extended glob" $ do
      writeFile [relfile|config/fourmolu/x/y.yaml|] ""
      matchAnyInCurrentDirectory globs `shouldReturn` True
