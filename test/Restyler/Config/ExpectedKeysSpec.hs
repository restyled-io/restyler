{-# LANGUAGE OverloadedStrings #-}

module Restyler.Config.ExpectedKeysSpec
    ( spec
    )
where

import SpecHelper

import Restyler.Config.ExpectedKeys

spec :: Spec
spec = do
    describe "validateExpectedKeyBy" $ do
        let validate = validateExpectedKeyBy "key" id

        it "accepts expected keys" $ do
            validate ["foo"] "foo" `shouldBe` Right "foo"
            validate ["foo", "bar"] "foo" `shouldBe` Right "foo"
            validate ["bar", "foo"] "foo" `shouldBe` Right "foo"
            validate ["foo", "foo"] "foo" `shouldBe` Right "foo"

        it "rejects with a default message" $ do
            let
                msg =
                    "Unexpected key \"bxx\", must be one of [\"foo\",\"bar\"]."

            validate ["foo", "bar"] "bxx" `shouldBe` Left msg

        it "suggests based on edit-distance" $ do
            let msg = "Unexpected key \"baz\", did you mean \"bar\"?"

            validate ["foo", "bar"] "baz" `shouldBe` Left msg
