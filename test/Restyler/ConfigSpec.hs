{-# LANGUAGE LambdaCase #-}

module Restyler.ConfigSpec
    ( spec
    )
where

import SpecHelper

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List (isInfixOf)
import Data.Yaml (decodeThrow, prettyPrintParseException)
import Restyler.Config
import Restyler.Config.Include
import Restyler.Restyler

spec :: Spec
spec = do
    it "supports a simple, name-based syntax" $ do
        defaultConfig <- getDefaultConfig

        result <- resolvedConfig
            $ C8.unlines ["---", "- stylish-haskell", "- prettier"]

        result `shouldBe` Right defaultConfig
            { cRestylers =
                [ someRestyler { rName = "stylish-haskell" }
                , someRestyler { rName = "prettier" }
                ]
            }

    it "has a setting for globally disabling" $ do
        result <- resolvedConfig $ C8.unlines
            ["---", "enabled: false", "restylers:", "- stylish-haskell"]

        fmap cEnabled result `shouldBe` Right False

    it "allows re-configuring includes" $ do
        defaultConfig <- getDefaultConfig

        result1 <- resolvedConfig $ C8.unlines
            ["---", "- stylish-haskell:", "    include:", "    - \"**/*.lhs\""]
        result2 <- resolvedConfig $ C8.unlines
            ["---", "- stylish-haskell:", "    include:", "    - \"**/*.lhs\""]
        result3 <- resolvedConfig $ C8.unlines
            [ "---"
            , "restylers:"
            , "- stylish-haskell:"
            , "    include:"
            , "    - \"**/*.lhs\""
            ]

        result1 `shouldBe` result2
        result2 `shouldBe` result3
        result3 `shouldBe` Right defaultConfig
            { cRestylers =
                [ someRestyler
                      { rName = "stylish-haskell"
                      , rInclude = [Include "**/*.lhs"]
                      }
                ]
            }

    it "has good errors for unknown name" $ do
        result1 <- resolvedConfig $ C8.unlines ["---", "- uknown-name"]
        result2 <- resolvedConfig $ C8.unlines
            ["---", "- uknown-name:", "    arguments:", "    - --foo"]
        result3 <- resolvedConfig $ C8.unlines
            [ "---"
            , "restylers:"
            , "- uknown-name:"
            , "    arguments:"
            , "    - --foo"
            ]

        result1 `shouldSatisfy` hasError
            "Unexpected Restyler name \"uknown-name\""
        result2 `shouldSatisfy` hasError
            "Unexpected Restyler name \"uknown-name\""
        result3 `shouldSatisfy` hasError
            "Unexpected Restyler name \"uknown-name\""

    it "provides suggestions for close matches" $ do
        result1 <- resolvedConfig $ C8.unlines ["---", "- hindex"]
        result2 <- resolvedConfig
            $ C8.unlines ["---", "- hindex:", "    arguments:", "    - --foo"]
        result3 <- resolvedConfig $ C8.unlines
            ["---", "restylers:", "- hindex:", "    arguments:", "    - --foo"]

        result1 `shouldSatisfy` hasError ", did you mean \"hindent\"?"
        result2 `shouldSatisfy` hasError ", did you mean \"hindent\"?"
        result3 `shouldSatisfy` hasError ", did you mean \"hindent\"?"

hasError :: String -> Either String a -> Bool
hasError msg (Left err) = msg `isInfixOf` err
hasError _ _ = False

getDefaultConfig :: MonadIO m => m Config
getDefaultConfig = runRIO testApp $ do
    config <- decodeThrow defaultConfigContent
    resolveRestylers config testRestylers

resolvedConfig :: MonadIO m => ByteString -> m (Either String Config)
resolvedConfig content =
    runRIO testApp $ fmap (first showConfigError) $ try $ do
        config <-
            resolveConfig
            <$> decodeThrow content
            <*> decodeThrow defaultConfigContent
        resolveRestylers config testRestylers

showConfigError :: ConfigError -> String
showConfigError = \case
    ConfigErrorInvalidYaml ex -> prettyPrintParseException ex
    ConfigErrorInvalidRestylers es -> unlines es
    ConfigErrorNoRestylers -> "No restylers"

testRestylers :: [Restyler]
testRestylers =
    [ someRestyler { rName = "astyle" }
    , someRestyler { rName = "autopep8" }
    , someRestyler { rName = "elm-format" }
    , someRestyler { rName = "hindent" }
    , someRestyler { rName = "php-cs-fixer" }
    , someRestyler { rName = "prettier" }
    , someRestyler { rName = "rubocop" }
    , someRestyler { rName = "rustfmt" }
    , someRestyler { rName = "shfmt" }
    , someRestyler { rName = "stylish-haskell" }
    , someRestyler { rName = "terraform" }
    ]
