{-# LANGUAGE OverloadedStrings #-}

module Restyler.LoggerSpec
    ( spec
    ) where

import SpecHelper

import Restyler.Logger

spec :: Spec
spec = do
    describe "splitLogStr" $ do
        it "splits known log levels" $ do
            splitLogStr "[Debug] Foo bar baz"
                `shouldBe` (Just "Debug", "Foo bar baz")

        it "handles trailing content" $ do
            splitLogStr "[Info#123] Foo bar baz"
                `shouldBe` (Just "Info#123", "Foo bar baz")

        it "handles non-terminated content" $ do
            splitLogStr "[Nope nope" `shouldBe` (Nothing, "[Nope nope")
