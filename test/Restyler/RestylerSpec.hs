-- |
--
-- Module      : Restyler.RestylerSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.RestylerSpec
  ( spec
  )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Lens
import Restyler.Restyler (upgradeEnabled)
import Test.Hspec

spec :: Spec
spec = do
  let
    upgradedValue :: Text -> Maybe Bool -> Value
    upgradedValue name =
      upgradeEnabled . object . \case
        Nothing -> ["name" .= name]
        Just enabled -> ["name" .= name, "enabled" .= enabled]

  describe "upgradeEnabled" $ do
    it "upgrades missing enabled key based on name" $ do
      let
        stylish = upgradedValue "stylish-haskell" Nothing
        hindent = upgradedValue "hindent" Nothing

      stylish ^? key "enabled" . _Bool `shouldBe` Just True
      hindent ^? key "enabled" . _Bool `shouldBe` Just False

    it "respects configs that have the key already" $ do
      let
        stylish = upgradedValue "stylish-haskell" $ Just False
        hindent = upgradedValue "hindent" $ Just True

      stylish ^? key "enabled" . _Bool `shouldBe` Just False
      hindent ^? key "enabled" . _Bool `shouldBe` Just True
