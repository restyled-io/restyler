module Restyler.Config.ExpectedKeys
    ( genericParseJSONValidated
    , validateObjectKeys
    , validateExpectedKeyBy
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
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
        let keys = map (fieldLabelModifier opts) $ selectors (Proxy @(Rep a))
        validateObjectKeys keys o
        genericParseJSON opts v
    v -> genericParseJSON opts v

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
    toParser :: MonadFail m => [String] -> m ()
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
