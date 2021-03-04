{-# LANGUAGE QuasiQuotes #-}

module Restyler.ConfigSpec
    ( spec
    ) where

import SpecHelper

import Data.List (isInfixOf)
import Restyler.App.Error
import Restyler.Capabilities.System
import Restyler.Config
import Restyler.Config.Include
import Restyler.Restyler
import Restyler.TestApp
import Text.Shakespeare.Text (st)

spec :: Spec
spec = do
    it "supports a simple, name-based syntax" $ runTestApp $ do
        defaultConfig <- loadDefaultConfig

        result <- loadConfigText [st|
            ---
            - stylish-haskell
            - prettier
        |]

        result `shouldBe` defaultConfig
            { cRestylers =
                [ someRestyler { rName = "stylish-haskell" }
                , someRestyler { rName = "prettier" }
                ]
            }

    it "has a setting for globally disabling" $ runTestApp $ do
        stageManifest "stable" testRestylers

        result <- loadConfigText [st|
            ---
            enabled: false
            restylers:
            - stylish-haskell
        |]

        cEnabled result `shouldBe` False

    it "allows re-configuring includes" $ runTestApp $ do
        defaultConfig <- loadDefaultConfig

        result1 <- loadConfigText [st|
            ---
            - stylish-haskell:
                include:
                - "**/*.lhs"
        |]
        result2 <- loadConfigText [st|
            ---
            - stylish-haskell:
                include:
                - "**/*.lhs"
        |]
        result3 <- loadConfigText [st|
            ---
            restylers:
            - stylish-haskell:
                include:
                - "**/*.lhs"
        |]

        result1 `shouldBe` result2
        result2 `shouldBe` result3
        result3 `shouldBe` defaultConfig
            { cRestylers =
                [ someRestyler
                      { rName = "stylish-haskell"
                      , rInclude = [Include "**/*.lhs"]
                      }
                ]
            }

    it "has good errors for unknown name" $ runTestApp $ do
        stageManifest "stable" testRestylers

        result1 <- tryError $ loadConfigText [st|
            ---
            - uknown-name
        |]
        result2 <- tryError $ loadConfigText [st|
            ---
            - uknown-name:
                arguments:
                - --foo
        |]
        result3 <- tryError $ loadConfigText [st|
            ---
            restylers:
            - uknown-name:
                arguments:
                - --foo
        |]

        result1 `shouldSatisfy` hasError
            "Unexpected Restyler name \"uknown-name\""
        result2 `shouldSatisfy` hasError
            "Unexpected Restyler name \"uknown-name\""
        result3 `shouldSatisfy` hasError
            "Unexpected Restyler name \"uknown-name\""

    it "reports multiple unknown names at once" $ runTestApp $ do
        stageManifest "stable" testRestylers

        result <- tryError $ loadConfigText [st|
            ---
            - unknown-name-1
            - unknown-name-2
        |]

        result `shouldSatisfy` hasError
            "Unexpected Restyler name \"unknown-name-1\""
        result `shouldSatisfy` hasError
            "Unexpected Restyler name \"unknown-name-2\""

    it "provides suggestions for close matches" $ runTestApp $ do
        stageManifest "stable" testRestylers

        result1 <- tryError $ loadConfigText [st|
            ---
            - hindex
        |]
        result2 <- tryError $ loadConfigText [st|
            ---
            - hindex:
                arguments:
                - --foo
        |]
        result3 <- tryError $ loadConfigText [st|
            ---
            restylers:
            - hindex:
                arguments:
                - --foo
        |]

        result1 `shouldSatisfy` hasError ", did you mean \"hindent\"?"
        result2 `shouldSatisfy` hasError ", did you mean \"hindent\"?"
        result3 `shouldSatisfy` hasError ", did you mean \"hindent\"?"

    it "doesn't loop on empty overrides" $ runTestApp $ do
        stageManifest "stable" testRestylers

        loadConfigText [st|
          - hindent: {}
        |]

    it "can specify a Restyler with name" $ runTestApp $ do
        defaultConfig <- loadDefaultConfig

        result <- loadConfigText [st|
            restylers:
              - name: hindent
                image: restyled/restyler-foo
                command: [foo]
                arguments: []
                include:
                  - "**/*.js"
                  - "**/*.jsx"
        |]

        result `shouldBe` defaultConfig
            { cRestylers =
                [ someRestyler
                      { rEnabled = True
                      , rName = "hindent"
                      , rImage = "restyled/restyler-foo"
                      , rCommand = ["foo"]
                      , rArguments = []
                      , rInclude = [Include "**/*.js", Include "**/*.jsx"]
                      }
                ]
            }

    it "supports * to indicate all other Restylers" $ runTestApp $ do
        stageManifest "stable" testRestylers

        result <- loadConfigText [st|
            restylers:
            - jdt
            - "*"
        |]

        map (rName &&& rEnabled) (cRestylers result)
            `shouldBe` [ ("jdt", True)
                       , ("astyle", True)
                       , ("autopep8", True)
                       , ("black", True)
                       , ("dfmt", True)
                       , ("elm-format", True)
                       , ("hindent", False)
                       , ("pg_format", True)
                       , ("php-cs-fixer", True)
                       , ("prettier", True)
                       , ("prettier-markdown", True)
                       , ("prettier-ruby", True)
                       , ("prettier-yaml", True)
                       , ("reorder-python-imports", True)
                       , ("rubocop", True)
                       , ("rustfmt", True)
                       , ("shellharden", True)
                       , ("shfmt", True)
                       , ("stylish-haskell", True)
                       , ("terraform", True)
                       , ("yapf", True)
                       ]

    it "can place * anywhere" $ runTestApp $ do
        stageManifest "stable" testRestylers

        result <- loadConfigText [st|
            restylers:
            - jdt
            - "*"
            - autopep8
        |]

        map (rName &&& rEnabled) (cRestylers result)
            `shouldBe` [ ("jdt", True)
                       , ("astyle", True)
                       , ("black", True)
                       , ("dfmt", True)
                       , ("elm-format", True)
                       , ("hindent", False)
                       , ("pg_format", True)
                       , ("php-cs-fixer", True)
                       , ("prettier", True)
                       , ("prettier-markdown", True)
                       , ("prettier-ruby", True)
                       , ("prettier-yaml", True)
                       , ("reorder-python-imports", True)
                       , ("rubocop", True)
                       , ("rustfmt", True)
                       , ("shellharden", True)
                       , ("shfmt", True)
                       , ("stylish-haskell", True)
                       , ("terraform", True)
                       , ("yapf", True)
                       , ("autopep8", True)
                       ]

    it "errors for more than one *" $ runTestApp $ do
        stageManifest "stable" testRestylers

        result <- tryError $ loadConfigText [st|
            restylers:
            - "*"
            - jdt
            - "*"
        |]

        result `shouldSatisfy` hasError "1 wildcard"

    it "handles invalid indentation nicely" $ runTestApp $ do
        result <- tryError $ loadConfigText [st|
            restylers:
              - prettier:
                include:
                  - "**/*.js"
                  - "**/*.jsx"
        |]

        result `shouldSatisfy` hasError "Do you have incorrect indentation"

    it "handles tabs nicely" $ runTestApp $ do
        stageManifest "stable" testRestylers
        result <- tryError $ loadConfigText [st|
            statuses:
            	differences: false
        |]

        result `shouldSatisfy` hasError "containing tabs"

    it "handles no configuration" $ runTestApp $ do
        defaultConfig <- loadDefaultConfig

        config <- loadConfigFrom $ map ConfigPath configPaths

        config `shouldBe` defaultConfig

    it "tries alternative configurations" $ runTestApp $ do
        stageManifest "stable" testRestylers
        writeFile "/a" "enabled: true\n"
        writeFile "/b" "enabled: false\n"

        aConfig <- loadConfigFrom [ConfigPath "/a", ConfigPath "/b"]
        bConfig <- loadConfigFrom [ConfigPath "/x", ConfigPath "/b"]

        cEnabled aConfig `shouldBe` True
        cEnabled bConfig `shouldBe` False

    it "doesn't skip configurations if there are errors" $ runTestApp $ do
        stageManifest "stable" testRestylers
        writeFile "/a" "{[^\n"
        writeFile "/b" "enabled: false\n"

        result <- tryError $ loadConfigFrom [ConfigPath "/a", ConfigPath "/b"]

        result `shouldSatisfy` hasError "trouble with your configuration"

    it "doesn't attempt to read unused configurations" $ runTestApp $ do
        stageManifest "stable" testRestylers
        writeFile "/a" "enabled: false\n"
        writeFile "/b" "[{^"

        config <- loadConfigFrom [ConfigPath "/a", ConfigPath "/b"]

        cEnabled config `shouldBe` False

hasError :: String -> Either AppError a -> Bool
hasError msg (Left err) = msg `isInfixOf` prettyAppError err
hasError _ _ = False
