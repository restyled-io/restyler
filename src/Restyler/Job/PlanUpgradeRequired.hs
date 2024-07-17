module Restyler.Job.PlanUpgradeRequired
  ( PlanUpgradeRequired (..)
  ) where

import Restyler.Prelude

import Restyler.Wiki qualified as Wiki

data PlanUpgradeRequired = PlanUpgradeRequired Text (Maybe URL)
  deriving stock (Eq, Show)

instance Exception PlanUpgradeRequired where
  displayException (PlanUpgradeRequired message mUpgradeUrl) =
    unpack
      $ message
      <> "\nFor additional help, please see: "
      <> Wiki.commonError "Plan Upgrade Required"
      <> maybe
        ""
        (("\nYou can upgrade your plan at " <>) . getUrl)
        mUpgradeUrl
