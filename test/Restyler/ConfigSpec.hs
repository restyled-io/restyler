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
        defaultConfig <- loadDefaultConfig

        result <- loadTestConfig
            $ C8.unlines ["---", "- stylish-haskell", "- prettier"]

        result `shouldBe` Right defaultConfig
            { cRestylers =
                [ someRestyler { rName = "stylish-haskell" }
                , someRestyler { rName = "prettier" }
                ]
            }

    it "has a setting for globally disabling" $ do
        result <- loadTestConfig $ C8.unlines
            ["---", "enabled: false", "restylers:", "- stylish-haskell"]

        fmap cEnabled result `shouldBe` Right False

    it "allows re-configuring includes" $ do
        defaultConfig <- loadDefaultConfig

        result1 <- loadTestConfig $ C8.unlines
            ["---", "- stylish-haskell:", "    include:", "    - \"**/*.lhs\""]
        result2 <- loadTestConfig $ C8.unlines
            ["---", "- stylish-haskell:", "    include:", "    - \"**/*.lhs\""]
        result3 <- loadTestConfig $ C8.unlines
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
        result1 <- loadTestConfig $ C8.unlines ["---", "- uknown-name"]
        result2 <- loadTestConfig $ C8.unlines
            ["---", "- uknown-name:", "    arguments:", "    - --foo"]
        result3 <- loadTestConfig $ C8.unlines
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
        result1 <- loadTestConfig $ C8.unlines ["---", "- hindex"]
        result2 <- loadTestConfig
            $ C8.unlines ["---", "- hindex:", "    arguments:", "    - --foo"]
        result3 <- loadTestConfig $ C8.unlines
            ["---", "restylers:", "- hindex:", "    arguments:", "    - --foo"]

        result1 `shouldSatisfy` hasError ", did you mean \"hindent\"?"
        result2 `shouldSatisfy` hasError ", did you mean \"hindent\"?"
        result3 `shouldSatisfy` hasError ", did you mean \"hindent\"?"

    it "can specify a full Restyler" $ do
        result <- loadTestConfig $ C8.unlines
            [ "restylers:"
            , "  - name: foo"
            , "    image: restyled/restyler-foo"
            , "    command: [foo]"
            , "    arguments: []"
            , "    include: []"
            , "    interpreters: []"
            , "    supports_arg_sep: false"
            , "    supports_multiple_paths: false"
            , "    documentation: []"
            , "    include:"
            , "      - \"**/*.js\""
            , "      - \"**/*.jsx\""
            ]

        result `shouldSatisfy` isRight

    it "handles invalid indentation nicely" $ do
        result <- loadTestConfig $ C8.unlines
            [ "restylers:"
            , "  - prettier:"
            , "    include:"
            , "      - \"**/*.js\""
            , "      - \"**/*.jsx\""
            ]

        result `shouldSatisfy` hasError "do you have incorrect indentation"

    it "handles tabs nicely" $ do
        result <- loadTestConfig
            $ C8.unlines ["statuses:", "\tdifferences: false"]

        result `shouldSatisfy` hasError "contains tabs"

hasError :: String -> Either String a -> Bool
hasError msg (Left err) = msg `isInfixOf` err
hasError _ _ = False

-- | Load just the default config, for comparisons against examples
loadDefaultConfig :: MonadIO m => m Config
loadDefaultConfig = runRIO testApp $ do
    config <- decodeThrow defaultConfigContent
    resolveRestylers config testRestylers

-- | Load a @'ByteString'@ as configuration
loadTestConfig :: MonadIO m => ByteString -> m (Either String Config)
loadTestConfig content =
    runRIO testApp
        $ tryTo showConfigError
        $ loadConfigFrom (ConfigContent content)
        $ const
        $ pure testRestylers

showConfigError :: ConfigError -> String
showConfigError = \case
    ConfigErrorInvalidYaml _ ex -> prettyPrintParseException ex
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
