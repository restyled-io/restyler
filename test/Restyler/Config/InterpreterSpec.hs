module Restyler.Config.InterpreterSpec
    ( spec
    )
where

import SpecHelper

import qualified Data.Text as T
import Restyler.Config.Interpreter

spec :: Spec
spec = do
    describe "hasIntepreter" $ do
        it "can find a direct executable" $ do
            let contents = T.unlines ["#!/bin/sh", "echo 1"]

            (contents `hasInterpreter` Sh) `shouldBe` True
            (contents `hasInterpreter` Bash) `shouldBe` False

        it "can find a nested executable" $ do
            let contents = T.unlines ["#!/usr/local/bin/bash", "echo 1"]

            (contents `hasInterpreter` Bash) `shouldBe` True

        it "can find something via /usr/bin/env" $ do
            let contents = T.unlines ["#!/usr/bin/env python", "print 1"]

            (contents `hasInterpreter` Python) `shouldBe` True
