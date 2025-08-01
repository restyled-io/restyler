module Restyler.Config.AutoEnable
  ( -- * Configuration
    AutoEnable (..)
  , AutoEnableGroup (..)

    -- * Result
  , AutoEnableResult (..)
  , toAutoEnableResults
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.Foldable1 (maximumBy)
import Data.List.NonEmpty qualified as NE
import Restyler.Config.Glob
import Restyler.Monad.Directory

data AutoEnable = AutoEnable
  { config_patterns :: Maybe [Glob FilePath]
  , group :: Maybe AutoEnableGroup
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data AutoEnableGroup = AutoEnableGroup
  { name :: Text
  , priority :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data AutoEnableResult
  = AutoEnableSome Text (NonEmpty Text)
  | AutoEnableNone

toAutoEnableResults
  :: MonadDirectory m
  => [Text]
  -- ^ Names of explicitly enabled restylers
  -> [(Text, AutoEnable)]
  -- ^ Pairs of restyler name and its auto-enable config
  -> m [AutoEnableResult]
toAutoEnableResults explicits restylers = do
  es <- traverse (uncurry $ toAutoEnableGroupElem explicits) restylers
  pure $ map toAutoEnableResult $ NE.groupAllWith (.autoEnableGroup.name) es

toAutoEnableResult :: NonEmpty AutoEnableGroupElem -> AutoEnableResult
toAutoEnableResult es = fromMaybe AutoEnableNone $ byConfig <|> byPriority
 where
  byConfig = do
    nes <- nonEmpty $ NE.filter (.hasConfig) es
    pure $ AutoEnableSome "config_patterns matched" $ NE.map (.name) nes

  byPriority = do
    guard $ none (.wasExplicit) es

    let message = case NE.length es of
          1 -> "restyler is in an auto_enable group"
          _ -> "restyler is highest priority in an auto_enable group"

    pure
      $ AutoEnableSome message
      $ pure
      $ (.name)
      $ maximumBy (comparing (.autoEnableGroup.priority)) es

data AutoEnableGroupElem = AutoEnableGroupElem
  { name :: Text
  , autoEnableGroup :: AutoEnableGroup
  , hasConfig :: Bool
  , wasExplicit :: Bool
  }

toAutoEnableGroupElem
  :: MonadDirectory m
  => [Text]
  -> Text
  -> AutoEnable
  -> m AutoEnableGroupElem
toAutoEnableGroupElem explicits name autoEnable = do
  hasConfig <- case autoEnable.config_patterns of
    Nothing -> pure False
    Just ps -> matchAnyInCurrentDirectory ps
  pure AutoEnableGroupElem {name, autoEnableGroup, hasConfig, wasExplicit}
 where
  autoEnableGroup = fromMaybe (AutoEnableGroup name 0) autoEnable.group
  wasExplicit = name `elem` explicits
