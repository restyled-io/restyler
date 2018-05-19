{-# LANGUAGE OverloadedStrings #-}

module Restyler.ConfigSpec
    ( spec
    ) where

import SpecHelper

import qualified Data.ByteString.Char8 as C8
import Data.List (isInfixOf)
import Data.Yaml (decodeEither)
import Restyler.Config

spec :: Spec
spec = do
    it "supports a simple, name-based syntax" $ do
        let
            result = decodeEither
                $ C8.unlines ["---", "- stylish-haskell", "- prettier"]

        result `shouldBe` Right Config
            { cEnabled = True
            , cRestylers = [ unsafeNamedRestyler "stylish-haskell"
                           , unsafeNamedRestyler "prettier"
                           ]
            , cInclude = ["**/*"]
            }

    it "has a setting for globally disabling" $ do
        let
            result = decodeEither $ C8.unlines
                ["---", "enabled: false", "restylers:", "- stylish-haskell"]

        fmap cEnabled result `shouldBe` Right False

    it "has a setting for global include/excludes" $ do
        let
            result = decodeEither $ C8.unlines
                [ "---"
                , "enabled: false"
                , "restylers:"
                , "  - stylish-haskell"
                , "include:"
                , "  - \"haskell/**/*\""
                , "  - '!*.tmp'"
                ]

        fmap cInclude result
            `shouldBe` Right [Include "haskell/**/*", Negated "*.tmp"]

    it "allows re-configuring includes" $ do
        let
            result1 =
                decodeEither
                    $ C8.unlines
                          [ "---"
                          , "- stylish-haskell:"
                          , "    include:"
                          , "    - \"**/*.lhs\""
                          ]

            result2 =
                decodeEither
                    $ C8.unlines
                          [ "---"
                          , "- stylish-haskell:"
                          , "    include:"
                          , "    - \"**/*.lhs\""
                          ]

            result3 = decodeEither $ C8.unlines
                [ "---"
                , "restylers:"
                , "- stylish-haskell:"
                , "    include:"
                , "    - \"**/*.lhs\""
                ]

        result1 `shouldBe` result2
        result2 `shouldBe` result3
        result3 `shouldBe` Right Config
            { cEnabled = True
            , cRestylers = [ (unsafeNamedRestyler "stylish-haskell")
                                 { rInclude = [Include "**/*.lhs"]
                                 }
                           ]
            , cInclude = ["**/*"]
            }

    it "has good errors for unknown name" $ do
        let
            result1 = decodeEither $ C8.unlines ["---", "- uknown-name"]

            result2 = decodeEither $ C8.unlines
                ["---", "- uknown-name:", "    arguments:", "    - --foo"]

            result3 = decodeEither $ C8.unlines
                [ "---"
                , "restylers:"
                , "- uknown-name:"
                , "    arguments:"
                , "    - --foo"
                ]

        result1 `shouldSatisfy` hasError "Unknown restyler name: uknown-name"
        result2 `shouldSatisfy` hasError "Unknown restyler name: uknown-name"
        result3 `shouldSatisfy` hasError "Unknown restyler name: uknown-name"

hasError :: String -> Either String Config -> Bool
hasError msg (Left err) = msg `isInfixOf` err
hasError _ _ = False
