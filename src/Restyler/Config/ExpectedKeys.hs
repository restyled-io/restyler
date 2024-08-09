-- |
--
-- Module      : Restyler.Config.ExpectedKeys
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.ExpectedKeys
  ( genericParseJSONValidated
  , validateObjectKeys
  , validateExpectedKeyBy
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.Text qualified as T
import GHC.Generics
import GHC.Generics.Selectors
import Text.EditDistance

genericParseJSONValidated
  :: forall a
   . (Generic a, GFromJSON Zero (Rep a), Selectors (Rep a))
  => Options
  -> Value
  -> Parser a
genericParseJSONValidated opts = \case
  v@(Object o) -> do
    let keys =
          map (pack . fieldLabelModifier opts)
            $ selectors (Proxy @(Rep a))
    validateObjectKeys keys o
    genericParseJSON opts v
  v -> genericParseJSON opts v

-- | Validate there are no unexpected keys in an Object
--
-- This is provided for convenience in the most common use-case. For a more
-- flexible interface, see @'validateExpectedKeyBy'@.
validateObjectKeys :: [Text] -> KeyMap v -> Parser ()
validateObjectKeys ks =
  toParser
    . lefts
    . map (validateExpectedKeyBy "key" id ks . Key.toText)
    . KeyMap.keys
 where
  toParser :: MonadFail m => [Text] -> m ()
  toParser [] = pure ()
  toParser xs = fail $ unpack $ unlines $ map ("- " <>) xs

-- | Validate that a key is present in a list of (projected) items
--
-- Returns the item found when validation passes.
validateExpectedKeyBy
  :: Text
  -- ^ The label to show as /Unknown \<label> .../
  -> (a -> Text)
  -- ^ A function to project each valid value as a comparable key
  -> [a]
  -- ^ The input list of valid items
  -> Text
  -- ^ The input key
  -> Either Text a
validateExpectedKeyBy label f as k = note msg $ find ((== k) . f) as
 where
  ks = map f as
  msg =
    "Unexpected "
      <> label
      <> " "
      <> show k
      <> ", "
      <> maybe
        ("must be one of " <> show ks <> ".")
        (("did you mean " <>) . (<> "?") . show)
        ( do
            (k', d) <- nearestElem k ks
            guard $ d <= T.length k `div` 2
            pure k'
        )

nearestElem :: Text -> [Text] -> Maybe (Text, Int)
nearestElem x = minimumByMaybe (compare `on` snd) . map (id &&& editDistance x)

editDistance :: Text -> Text -> Int
editDistance a b = levenshteinDistance defaultEditCosts (unpack a) (unpack b)
