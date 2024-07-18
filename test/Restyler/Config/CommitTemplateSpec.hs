module Restyler.Config.CommitTemplateSpec
  ( spec
  )
where

import SpecHelper

import Restyler.Config.CommitTemplate

spec :: Spec
spec = do
  describe "renderCommitTemplate" $ do
    it "replaces variables" $ example $ do
      let
        inputs = CommitTemplateInputs {restyler = someRestyler "special"}
        template = CommitTemplate "Restyled by ${restyler.name}"

      renderCommitTemplate inputs template
        `shouldBe` "Restyled by special"
