module Restyler.CommitTemplateSpec
  ( spec
  )
where

import SpecHelper

import Restyler.CommitTemplate

spec :: Spec
spec = do
  describe "renderCommitTemplate" $ do
    it "replaces variables" $ example $ do
      let
        inputs =
          CommitTemplateInputs
            { ctiRestyler = someRestyler "special"
            }
        template = commitTemplate "Restyled by ${restyler.name}"

      renderCommitTemplate inputs template
        `shouldBe` "Restyled by special"
