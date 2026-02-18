{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- Module      : Restyler.Restyler.RunSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Restyler.RunSpec
  ( spec
  ) where

import Restyler.Prelude

import Path (relfile)
import Restyler.Config.Interpreter
import Restyler.Restyler
import Restyler.Restyler.Run
import Restyler.Test.App
import Restyler.Test.Fixtures (someRestyler)

spec :: Spec
spec = withSimpleTestApp $ do
  describe "withFilteredPaths" $ do
    it "does not bring excluded files back by shebang" $ do
      let
        a = [relfile|a|]
        b = [relfile|b|]
      writeFile a "#!/bin/sh\necho A\n"
      modifyPermissions a $ \p -> p {executable = True}
      writeFile b "#!/bin/sh\necho B\n"
      modifyPermissions b $ \p -> p {executable = True}

      filtered <-
        withFilteredPaths
          [ (someRestyler "") {rInclude = ["**/*.sh"], rInterpreters = [Sh]}
          , (someRestyler "")
              { rInclude = ["**/*.sh", "!b"]
              , rInterpreters = [Sh]
              }
          ]
          [a, b]
          (const $ pure . Just)

      filtered `shouldBe` Just ([a, b] :| [[a]])
