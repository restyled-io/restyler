{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Restyler.ConfigSpec
    ( spec
    )
where

import SpecHelper

import Data.List (isInfixOf)
import qualified Data.Text as T
import Data.Yaml (prettyPrintParseException)
import Restyler.Config
import Restyler.Config.Include
import Restyler.Restyler
import Text.Shakespeare.Text (st)

spec :: Spec
spec = do
    it "supports a simple, name-based syntax" $ example $ do
        defaultConfig <- loadDefaultConfig'

        result <- loadTestConfig [st|
            ---
            - stylish-haskell
            - prettier
        |]

        result `shouldBe` Right defaultConfig
            { cRestylers =
                [ someRestyler { rName = "stylish-haskell" }
                , someRestyler { rName = "prettier" }
                ]
            }

    it "has a setting for globally disabling" $ example $ do
        result <- loadTestConfig [st|
            ---
            enabled: false
            restylers:
            - stylish-haskell
        |]

        fmap cEnabled result `shouldBe` Right False

    it "allows re-configuring includes" $ example $ do
        defaultConfig <- loadDefaultConfig'

        result1 <- loadTestConfig [st|
            ---
            - stylish-haskell:
                include:
                - "**/*.lhs"
        |]
        result2 <- loadTestConfig [st|
            ---
            - stylish-haskell:
                include:
                - "**/*.lhs"
        |]
        result3 <- loadTestConfig [st|
            ---
            restylers:
            - stylish-haskell:
                include:
                - "**/*.lhs"
        |]

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

    it "has good errors for unknown name" $ example $ do
        result1 <- loadTestConfig [st|
            ---
            - uknown-name
        |]
        result2 <- loadTestConfig [st|
            ---
            - uknown-name:
                arguments:
                - --foo
        |]
        result3 <- loadTestConfig [st|
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

    it "reports multiple unknown names at once" $ example $ do
        result <- loadTestConfig [st|
            ---
            - unknown-name-1
            - unknown-name-2
        |]

        result `shouldSatisfy` hasError
            "Unexpected Restyler name \"unknown-name-1\""
        result `shouldSatisfy` hasError
            "Unexpected Restyler name \"unknown-name-2\""

    it "provides suggestions for close matches" $ example $ do
        result1 <- loadTestConfig [st|
            ---
            - hindex
        |]
        result2 <- loadTestConfig [st|
            ---
            - hindex:
                arguments:
                - --foo
        |]
        result3 <- loadTestConfig [st|
            ---
            restylers:
            - hindex:
                arguments:
                - --foo
        |]

        result1 `shouldSatisfy` hasError ", did you mean \"hindent\"?"
        result2 `shouldSatisfy` hasError ", did you mean \"hindent\"?"
        result3 `shouldSatisfy` hasError ", did you mean \"hindent\"?"

    it "doesn't loop on empty overrides" $ example $ do
        result <- loadTestConfig [st|
          - hindent: {}
        |]

        result `shouldSatisfy` isRight

    it "can specify a Restyler with name" $ example $ do
        defaultConfig <- loadDefaultConfig'

        result <- loadTestConfig [st|
            restylers:
              - name: hindent
                image: restyled/restyler-foo
                command: [foo]
                arguments: []
                include:
                  - "**/*.js"
                  - "**/*.jsx"
        |]

        result `shouldBe` Right defaultConfig
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

    it "supports * to indicate all other Restylers" $ example $ do
        result <- assertTestConfig [st|
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

    it "can place * anywhere" $ example $ do
        result <- assertTestConfig [st|
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

    it "errors for more than one *" $ example $ do
        result <- loadTestConfig [st|
            restylers:
            - "*"
            - jdt
            - "*"
        |]

        result `shouldSatisfy` hasError "1 wildcard"

    it "handles invalid indentation nicely" $ example $ do
        result <- loadTestConfig [st|
            restylers:
              - prettier:
                include:
                  - "**/*.js"
                  - "**/*.jsx"
        |]

        result `shouldSatisfy` hasError "Do you have incorrect indentation"

    it "handles tabs nicely" $ example $ do
        result <- loadTestConfig [st|
            statuses:
            	differences: false
        |]

        result `shouldSatisfy` hasError "containing tabs"

    it "handles no configuration" $ example $ do
        defaultConfig <- loadDefaultConfig'
        app <- liftIO $ testApp "/" []

        result <-
            runRIO app
            $ tryTo showConfigError
            $ loadConfigFrom (map ConfigPath configPaths)
            $ const
            $ pure testRestylers

        result `shouldBe` Right defaultConfig

    it "tries alternative configurations" $ example $ do
        app <- liftIO $ testApp
            "/"
            [("/a", "enabled: true\n"), ("/b", "enabled: false\n")]

        aConfig <-
            runRIO app
            $ tryTo showConfigError
            $ loadConfigFrom [ConfigPath "/a", ConfigPath "/b"]
            $ const
            $ pure testRestylers

        bConfig <-
            runRIO app
            $ tryTo showConfigError
            $ loadConfigFrom [ConfigPath "/x", ConfigPath "/b"]
            $ const
            $ pure testRestylers

        fmap cEnabled aConfig `shouldBe` Right True
        fmap cEnabled bConfig `shouldBe` Right False

    it "doesn't skip configurations if there are errors" $ example $ do
        app <- liftIO
            $ testApp "/" [("/a", "{[^\n"), ("/b", "enabled: false\n")]

        result <-
            runRIO app
            $ tryTo showConfigError
            $ loadConfigFrom [ConfigPath "/a", ConfigPath "/b"]
            $ const
            $ pure testRestylers

        result `shouldSatisfy` isLeft

    it "doesn't attempt to read unused configurations" $ example $ do
        app <- liftIO $ testApp "/" [("/a", "enabled: false\n"), ("/b", "[{^")]

        result <-
            runRIO app
            $ tryTo showConfigError
            $ loadConfigFrom [ConfigPath "/a", ConfigPath "/b"]
            $ const
            $ pure testRestylers

        fmap cEnabled result `shouldBe` Right False

hasError :: String -> Either String a -> Bool
hasError msg (Left err) = msg `isInfixOf` err
hasError _ _ = False

-- | Load just the default config, for comparisons against examples
loadDefaultConfig' :: MonadIO m => m Config
loadDefaultConfig' = do
    app <- liftIO $ testApp "/" []
    runRIO app loadDefaultConfig

-- | Load a @'Text'@ as configuration
loadTestConfig :: MonadIO m => Text -> m (Either String Config)
loadTestConfig content = do
    app <- liftIO $ testApp "/" []
    runRIO app
        $ tryTo showConfigError
        $ loadConfigFrom [ConfigContent $ encodeUtf8 $ dedent content]
        $ const
        $ pure testRestylers

-- | Load a @'Text'@ as configuration, fail on errors
assertTestConfig :: MonadIO m => Text -> m Config
assertTestConfig = either throwString pure <=< loadTestConfig

showConfigError :: ConfigError -> String
showConfigError = \case
    ConfigErrorInvalidYaml yaml ex ->
        unlines [prettyPrintParseException ex, "---", show yaml]
    ConfigErrorInvalidRestylers errs -> unlines errs
    ConfigErrorInvalidRestylersYaml ex -> show ex

dedent :: Text -> Text
dedent x = T.unlines $ map (T.drop indent) ls
  where
    ls = T.lines $ T.dropWhileEnd isSpace $ T.dropWhile (== '\n') x
    indent = fromMaybe 0 $ minimumMaybe indents
    indents = map (T.length . T.takeWhile (== ' ')) ls
