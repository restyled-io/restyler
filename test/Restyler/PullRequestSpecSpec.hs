{-# LANGUAGE OverloadedStrings #-}

module Restyler.PullRequestSpecSpec
    ( spec
    )
where

import SpecHelper

import qualified Data.Text as T
import Restyler.PullRequestSpec
import Test.QuickCheck

spec :: Spec
spec = describe "parseSpec" $ do
    it "parses correctly" $ do
        parseSpec "foo/bar#1" `shouldBe` Right (PullRequestSpec "foo" "bar" 1)
        parseSpec "baz/bat#2" `shouldBe` Right (PullRequestSpec "baz" "bat" 2)

    it "errors on invalid input" $ do
        parseSpec "foo/bar" `shouldSatisfy` isLeft
        parseSpec "bar#2" `shouldSatisfy` isLeft
        parseSpec "foo/bar#baz" `shouldSatisfy` isLeft

    it "round-trips" $ property $ \(owner, name, Positive num) ->
        let prSpec = PullRequestSpec owner name num
        in parseSpec (T.unpack $ showSpec prSpec) == Right prSpec
