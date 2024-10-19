module Restyler.Config.RestylerSpec
  ( spec
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.Config.Restyler
import Restyler.Restyler
import Restyler.Test.Fixtures
import Test.Hspec

spec :: Spec
spec = do
  describe "overrideRestylers" $ do
    it "fails if wildcard is specified more than once" $ do
      let result =
            overrideRestylers
              []
              [ enabled "fourmolu"
              , wildcard
              , disabled "stylish-haskell"
              , wildcard
              ]

      result `shouldHaveFailure` "at most 1 wildcard"
      result `shouldHaveFailure` "2 found"

    it "fails for an unknown name" $ do
      let result =
            overrideRestylers
              [ someRestyler "brittany"
              , someRestyler "fourmolu"
              , someRestyler "hlint"
              , someRestyler "stylish-haskell"
              ]
              [ enabled "fourmolu"
              , disabled "stylish-haskell"
              , enabled "boppity-boopy"
              ]

      result `shouldHaveFailure` "Unexpected"
      result `shouldHaveFailure` "name boppity-boopy"

    it "places the remaining restylers where the wildcard was" $ do
      let
        restylers =
          [ someRestyler "brittany"
          , someRestyler "fourmolu"
          , someRestyler "hlint"
          , someRestyler "stylish-haskell"
          ]
        resultNone =
          overrideRestylers
            restylers
            [ enabled "fourmolu"
            , disabled "stylish-haskell"
            ]
        resultFirst =
          overrideRestylers
            restylers
            [ wildcard
            , enabled "fourmolu"
            , disabled "stylish-haskell"
            ]
        resultLast =
          overrideRestylers
            restylers
            [ enabled "fourmolu"
            , disabled "stylish-haskell"
            , wildcard
            ]

      resultNone
        `shouldBe` Right
          [ someRestyler "fourmolu"
          , (someRestyler "stylish-haskell") {rEnabled = False}
          ]
      resultFirst
        `shouldBe` Right
          [ someRestyler "brittany"
          , someRestyler "hlint"
          , someRestyler "fourmolu"
          , (someRestyler "stylish-haskell") {rEnabled = False}
          ]
      resultLast
        `shouldBe` Right
          [ someRestyler "fourmolu"
          , (someRestyler "stylish-haskell") {rEnabled = False}
          , someRestyler "brittany"
          , someRestyler "hlint"
          ]

shouldHaveFailure :: (Show a, HasCallStack) => Either [Text] a -> Text -> IO ()
shouldHaveFailure result needle = go result
 where
  go = \case
    Left (e : es)
      | needle `T.isInfixOf` e -> pure ()
      | otherwise -> go $ Left es
    _ ->
      expectationFailure
        $ "Expected: Left containing "
        <> show needle
        <> "."
        <> "\nActual: "
        <> show result

infix 1 `shouldHaveFailure`
