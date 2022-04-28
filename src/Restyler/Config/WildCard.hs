module Restyler.Config.WildCard
    ( WildCard
    , insertWildCard
    , overrideWildCard
    ) where

import Restyler.Prelude

import Data.Aeson hiding (Success)
import Data.Validation
import qualified RIO.HashMap as HashMap
import Restyler.Config.ExpectedKeys (validateExpectedKeyBy)

data WildCard a = Explicit a | WildCard

isWildCard :: WildCard a -> Bool
isWildCard = \case
    Explicit{} -> False
    WildCard -> True

explicits :: [WildCard a] -> [a]
explicits = concatMap $ \case
    Explicit a -> [a]
    WildCard -> []

instance FromJSON a => FromJSON (WildCard a) where
    parseJSON = \case
        String x | x == wildCardSigil -> pure WildCard
        o -> Explicit <$> parseJSON o

instance ToJSON a => ToJSON (WildCard a) where
    toJSON = \case
        Explicit a -> toJSON a
        WildCard -> toJSON wildCardSigil

    toEncoding = \case
        Explicit a -> toEncoding a
        WildCard -> toEncoding wildCardSigil

-- | Replace the WildCard with the elements given
insertWildCard :: Eq a => [a] -> [WildCard a] -> Either [String] [a]
insertWildCard as bs = toEither $ checkWildCards bs *> inserteds
  where
    inserteds = fmap concat $ for bs $ \case
        Explicit b -> pure [b]
        WildCard -> pure $ filter (`notElem` explicits bs) as

-- | A form of WildCard handling that overrides the base values
overrideWildCard
    :: String
    -> (a -> String)
    -- ^ Project the items to be overridden into a key
    -> (b -> String)
    -- ^ Project the items to override with into a key
    -> (a -> b -> a)
    -- ^ The function to do the override of an item
    -> [a]
    -- ^ The list of base items to start with
    -> [WildCard b]
    -- ^ The list of overrides or wildcard element
    -> Either [String] [a]
overrideWildCard label aToKey bToKey abToA as bs =
    toEither $ checkWildCards bs *> overrides
  where
    overrides = fmap concat $ for bs $ \case
        -- Return this a overriding its b
        Explicit b ->
            (\a -> [abToA a b]) <$> lookupExpectedKeyBy label asMap (bToKey b)

        -- Return any as not already present in bs as-is
        WildCard -> pure $ filter (\a -> aToKey a `notElem` bKeys) as

    asMap = HashMap.fromList $ map (aToKey &&& id) as
    bKeys = map bToKey $ explicits bs

checkWildCards :: [WildCard a] -> Validation [String] ()
checkWildCards bs = when (nWildCards > 1) $ Failure
    [ "WildCard lists can contain at most one "
      <> show wildCardSigil
      <> " element, saw "
      <> show nWildCards
    ]
    where nWildCards = length (filter isWildCard bs)

wildCardSigil :: Text
wildCardSigil = "*"

lookupExpectedKeyBy
    :: String -> HashMap String v -> String -> Validation [String] v
lookupExpectedKeyBy label hm k =
    case validateExpectedKeyBy label fst (HashMap.toList hm) k of
        Left e -> Failure [e]
        Right (_k, v) -> Success v
