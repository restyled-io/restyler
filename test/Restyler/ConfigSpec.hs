{-# LANGUAGE QuasiQuotes #-}

module Restyler.ConfigSpec
  ( spec
  ) where

import SpecHelper

import qualified Data.Text as T
import Data.Yaml (prettyPrintParseException)
import Restyler.Config
import Restyler.Config.Include
import Restyler.Restyler
import Text.Shakespeare.Text (st)
import UnliftIO.Exception (try)

spec :: Spec
spec = withTestApp $ do
  it "supports a simple, name-based syntax" $ testAppExample $ do
    defaultConfig <- loadDefaultConfig

    result <-
      loadTestConfig
        [st|
            ---
            - stylish-haskell
            - prettier
        |]

    result
      `shouldBe` Right
        defaultConfig
          { cRestylers =
              [ someRestyler {rName = "stylish-haskell"}
              , someRestyler {rName = "prettier"}
              ]
          }

  it "has a setting for globally disabling" $ testAppExample $ do
    result <-
      loadTestConfig
        [st|
            ---
            enabled: false
            restylers:
            - stylish-haskell
        |]

    fmap cEnabled result `shouldBe` Right False

  it "allows re-configuring includes" $ testAppExample $ do
    defaultConfig <- loadDefaultConfig

    result1 <-
      loadTestConfig
        [st|
            ---
            - stylish-haskell:
                include:
                - "**/*.lhs"
        |]
    result2 <-
      loadTestConfig
        [st|
            ---
            - stylish-haskell:
                include:
                - "**/*.lhs"
        |]
    result3 <-
      loadTestConfig
        [st|
            ---
            restylers:
            - stylish-haskell:
                include:
                - "**/*.lhs"
        |]

    result1 `shouldBe` result2
    result2 `shouldBe` result3
    result3
      `shouldBe` Right
        defaultConfig
          { cRestylers =
              [ someRestyler
                  { rName = "stylish-haskell"
                  , rInclude = [Include "**/*.lhs"]
                  }
              ]
          }

  it "has good errors for unknown name" $ testAppExample $ do
    result1 <-
      loadTestConfig
        [st|
            ---
            - uknown-name
        |]
    result2 <-
      loadTestConfig
        [st|
            ---
            - uknown-name:
                arguments:
                - --foo
        |]
    result3 <-
      loadTestConfig
        [st|
            ---
            restylers:
            - uknown-name:
                arguments:
                - --foo
        |]

    result1
      `shouldSatisfy` hasError
        "Unexpected Restyler name \"uknown-name\""
    result2
      `shouldSatisfy` hasError
        "Unexpected Restyler name \"uknown-name\""
    result3
      `shouldSatisfy` hasError
        "Unexpected Restyler name \"uknown-name\""

  it "reports multiple unknown names at once" $ testAppExample $ do
    result <-
      loadTestConfig
        [st|
            ---
            - unknown-name-1
            - unknown-name-2
        |]

    result
      `shouldSatisfy` hasError
        "Unexpected Restyler name \"unknown-name-1\""
    result
      `shouldSatisfy` hasError
        "Unexpected Restyler name \"unknown-name-2\""

  it "provides suggestions for close matches" $ testAppExample $ do
    result1 <-
      loadTestConfig
        [st|
            ---
            - hindex
        |]
    result2 <-
      loadTestConfig
        [st|
            ---
            - hindex:
                arguments:
                - --foo
        |]
    result3 <-
      loadTestConfig
        [st|
            ---
            restylers:
            - hindex:
                arguments:
                - --foo
        |]

    result1 `shouldSatisfy` hasError ", did you mean \"hindent\"?"
    result2 `shouldSatisfy` hasError ", did you mean \"hindent\"?"
    result3 `shouldSatisfy` hasError ", did you mean \"hindent\"?"

  it "doesn't loop on empty overrides" $ testAppExample $ do
    result <-
      loadTestConfig
        [st|
          - hindent: {}
        |]

    result `shouldSatisfy` isRight

  it "can specify a Restyler with name" $ testAppExample $ do
    defaultConfig <- loadDefaultConfig

    result <-
      loadTestConfig
        [st|
            restylers:
              - name: hindent
                image: restyled/restyler-foo
                command: [foo]
                arguments: []
                include:
                  - "**/*.js"
                  - "**/*.jsx"
        |]

    result
      `shouldBe` Right
        defaultConfig
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

  it "supports * to indicate all other Restylers" $ testAppExample $ do
    result <-
      assertTestConfig
        [st|
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

  it "can place * anywhere" $ testAppExample $ do
    result <-
      assertTestConfig
        [st|
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

  it "errors for more than one *" $ testAppExample $ do
    result <-
      loadTestConfig
        [st|
            restylers:
            - "*"
            - jdt
            - "*"
        |]

    result `shouldSatisfy` hasError "1 wildcard"

  it "handles invalid indentation nicely" $ testAppExample $ do
    result <-
      loadTestConfig
        [st|
            restylers:
              - prettier:
                include:
                  - "**/*.js"
                  - "**/*.jsx"
        |]

    result `shouldSatisfy` hasError "Do you have incorrect indentation"

  it "handles no configuration" $ testAppExample $ do
    defaultConfig <- loadDefaultConfig

    result <-
      tryTo showConfigError
        $ loadConfigFrom (map ConfigPath configPaths)
        $ const
        $ pure testRestylers

    result `shouldBe` Right defaultConfig

  it "tries alternative configurations" $ testAppExample $ do
    writeFile "/a" "enabled: true\n"
    writeFile "/b" "enabled: false\n"

    aConfig <-
      tryTo showConfigError
        $ loadConfigFrom [ConfigPath "/a", ConfigPath "/b"]
        $ const
        $ pure testRestylers

    bConfig <-
      tryTo showConfigError
        $ loadConfigFrom [ConfigPath "/x", ConfigPath "/b"]
        $ const
        $ pure testRestylers

    fmap cEnabled aConfig `shouldBe` Right True
    fmap cEnabled bConfig `shouldBe` Right False

  it "doesn't skip configurations if there are errors" $ testAppExample $ do
    writeFile "/a" "{[^\n"
    writeFile "/b" "enabled: false\n"

    result <-
      tryTo showConfigError
        $ loadConfigFrom [ConfigPath "/a", ConfigPath "/b"]
        $ const
        $ pure testRestylers

    result `shouldSatisfy` isLeft

  it "doesn't attempt to read unused configurations" $ testAppExample $ do
    writeFile "/a" "enabled: false\n"
    writeFile "/b" "[{^"

    result <-
      tryTo showConfigError
        $ loadConfigFrom [ConfigPath "/a", ConfigPath "/b"]
        $ const
        $ pure testRestylers

    fmap cEnabled result `shouldBe` Right False

tryTo :: (MonadUnliftIO m, Exception e) => (e -> b) -> m a -> m (Either b a)
tryTo f = fmap (first f) . try

hasError :: Text -> Either Text a -> Bool
hasError msg (Left err) = msg `T.isInfixOf` err
hasError _ _ = False

-- | Load a @'Text'@ as configuration
loadTestConfig
  :: (MonadUnliftIO m, MonadSystem m) => Text -> m (Either Text Config)
loadTestConfig content = do
  tryTo showConfigError
    $ loadConfigFrom [ConfigContent $ encodeUtf8 $ dedent content]
    $ const
    $ pure testRestylers

-- | Load a @'Text'@ as configuration, fail on errors
assertTestConfig :: (MonadUnliftIO m, MonadSystem m) => Text -> m Config
assertTestConfig = either (throwString . unpack) pure <=< loadTestConfig

showConfigError :: ConfigError -> Text
showConfigError = \case
  ConfigErrorInvalidYaml _path yaml ex ->
    unlines [pack $ prettyPrintParseException ex, "---", show yaml]
  ConfigErrorInvalidRestylers errs -> unlines errs
  ConfigErrorInvalidRestylersYaml ex -> show ex

dedent :: Text -> Text
dedent x = T.unlines $ map (T.drop indent) ls
 where
  ls = T.lines $ T.dropWhileEnd isSpace $ T.dropWhile (== '\n') x
  indent = fromMaybe 0 $ minimumMaybe indents
  indents = map (T.length . T.takeWhile (== ' ')) ls
