-- |
--
-- Module      : Restyler.Config.InterpreterSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.InterpreterSpec
  ( spec
  )
where

import SpecHelper

import Data.Text qualified as T
import Restyler.Config.Interpreter

spec :: Spec
spec = do
  describe "readInterpreter" $ do
    it "can find a direct executable" $ example $ do
      let contents = T.unlines ["#!/bin/sh", "echo 1"]

      readInterpreter contents `shouldBe` Just Sh

    it "can find a nested executable" $ example $ do
      let contents = T.unlines ["#!/usr/local/bin/bash", "echo 1"]

      readInterpreter contents `shouldBe` Just Bash

    it "can find something via /usr/bin/env" $ example $ do
      let contents = T.unlines ["#!/usr/bin/env python", "print 1"]

      readInterpreter contents `shouldBe` Just Python
