{-# LANGUAGE OverloadedStrings #-}

module Restyler.Config.ExpectedKeys
    ( validateObjectKeys
    , validateExpectedKeyBy
    ) where

import Restyler.Prelude

import Data.Aeson.Types (Parser)
import Data.Either (lefts)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Text.EditDistance

-- | Validate there are no unexpected keys in an Object
--
-- This is provided for convenience in the most common use-case. For a more
-- flexible interface, see @'validateExpectedKeyBy'@.
--
validateObjectKeys :: [String] -> HashMap Text v -> Parser ()
validateObjectKeys ks =
    toParser
        . lefts
        . map (validateExpectedKeyBy "key" id ks . unpack)
        . HM.keys
  where
    toParser [] = pure ()
    toParser xs = fail $ unlines $ map ("- " <>) xs

-- | Validate that a key is present in a list of (projected) items
--
-- Returns the item found when validation passes.
--
validateExpectedKeyBy
    :: String
    -- ^ The label to show as /Unknown \<label> .../
    -> (a -> String)
    -- ^ A function to project each valid value as a comparable key
    -> [a]
    -- ^ The input list of valid items
    -> String
    -- ^ The input key
    -> Either String a
validateExpectedKeyBy label f as k = note msg $ find ((== k) . f) as
  where
    ks = map f as
    msg = "Unexpected " <> label <> " " <> show k <> ", " <> maybe
        ("must be one of " <> show ks <> ".")
        (("did you mean " <>) . (<> "?") . show)
        (do
            (k', d) <- nearestElem k ks
            guard $ d <= length k `div` 2
            pure k'
        )

nearestElem :: String -> [String] -> Maybe (String, Int)
nearestElem x = minimumByMaybe (compare `on` snd) . map (id &&& editDistance x)

editDistance :: String -> String -> Int
editDistance = levenshteinDistance defaultEditCosts
