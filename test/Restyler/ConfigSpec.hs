{-# LANGUAGE QuasiQuotes #-}

module Restyler.ConfigSpec
    ( spec
    )
where

import SpecHelper

import Data.List (isInfixOf)
import qualified Data.Text as T
import Restyler.App.Error
import Restyler.Capabilities.DownloadFile
import Restyler.Capabilities.Logger
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

        result <- loadTestConfig [st|
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

        result <- loadTestConfig [st|
            ---
            enabled: false
            restylers:
            - stylish-haskell
        |]

        cEnabled result `shouldBe` False

    it "allows re-configuring includes" $ runTestApp $ do
        defaultConfig <- loadDefaultConfig

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

        result1 <- tryError $ loadTestConfig [st|
            ---
            - uknown-name
        |]
        result2 <- tryError $ loadTestConfig [st|
            ---
            - uknown-name:
                arguments:
                - --foo
        |]
        result3 <- tryError $ loadTestConfig [st|
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

        {- TODO
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
-}

hasError :: String -> Either AppError a -> Bool
hasError msg (Left err) = msg `isInfixOf` prettyAppError err
hasError _ _ = False

loadTestConfig
    :: ( MonadLogger m
       , MonadSystem m
       , MonadDownloadFile m
       , MonadError AppError m
       )
    => Text
    -> m Config
loadTestConfig content = do
    loadConfigFrom [ConfigContent $ encodeUtf8 $ dedent content]

dedent :: Text -> Text
dedent x = T.unlines $ map (T.drop indent) ls
  where
    ls = T.lines $ T.dropWhileEnd isSpace $ T.dropWhile (== '\n') x
    indent = fromMaybe 0 $ minimumMaybe indents
    indents = map (T.length . T.takeWhile (== ' ')) ls
