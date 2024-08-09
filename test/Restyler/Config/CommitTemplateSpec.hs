-- |
--
-- Module      : Restyler.Config.CommitTemplateSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
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
