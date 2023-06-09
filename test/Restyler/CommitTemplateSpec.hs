module Restyler.CommitTemplateSpec
  ( spec
  )
where

import SpecHelper

import Restyler.CommitTemplate
import Restyler.Restyler

spec :: Spec
spec = do
  describe "renderCommitTemplate" $ do
    it "replaces variables" $ example $ do
      let
        inputs =
          CommitTemplateInputs
            { ctiRestyler = someRestyler {rName = "special"}
            }
        template = commitTemplate "Restyled by ${restyler.name}"

      renderCommitTemplate inputs template
        `shouldBe` "Restyled by special"
