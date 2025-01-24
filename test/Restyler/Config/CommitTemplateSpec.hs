-- |
--
-- Module      : Restyler.Config.CommitTemplateSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.CommitTemplateSpec
  ( spec
  )
where

import Restyler.Prelude

import Restyler.Config.CommitTemplate
import Test.Hspec

spec :: Spec
spec = do
  describe "renderCommitTemplate" $ do
    it "replaces variables" $ do
      let
        inputs = CommitTemplateInputs {restyler = "special"}
        template = CommitTemplate "Restyled by ${restyler.name}"

      renderCommitTemplate inputs template
        `shouldBe` "Restyled by special"
