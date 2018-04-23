{-# LANGUAGE OverloadedStrings #-}
module Restyler.Config.InterpreterSpec
    ( spec
    ) where

import SpecHelper

import Control.Exception (bracket)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Restyler.Config.Interpreter
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)

spec :: Spec
spec = around withEmptySystemTempFile $ do
    describe "hasIntepreter" $ do
        it "can find a direct executable" $ \tmp -> do
            T.writeFile tmp $ T.unlines ["#!/bin/sh", "echo 1"]

            (tmp `hasInterpreter` Sh) `shouldReturn` True
            (tmp `hasInterpreter` Bash) `shouldReturn` False

        it "can find a nested executable" $ \tmp -> do
            T.writeFile tmp $ T.unlines ["#!/usr/local/bin/bash", "echo 1"]

            (tmp `hasInterpreter` Bash) `shouldReturn` True

        it "can find something via /usr/bin/env" $ \tmp -> do
            T.writeFile tmp $ T.unlines ["#!/usr/bin/env python", "print 1"]

            (tmp `hasInterpreter` Python) `shouldReturn` True


withEmptySystemTempFile :: (FilePath -> IO a) -> IO a
withEmptySystemTempFile = bracket (emptySystemTempFile "") removeFile
