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
import qualified Prelude as Unsafe

-- | Validate there are no unexpected keys in an Object
--
-- This is provided for convenience in the most common use-case. For a more
-- flexible interface, see @'validateExpectedKeyBy'@.
--
validateObjectKeys :: [String] -> HashMap Text v -> Parser ()
validateObjectKeys ks =
    toParser
        . lefts
        . map (validateExpectedKeyBy "key" id ks)
        . map unpack
        . HM.keys
  where
    toParser [] = pure ()
    toParser xs = fail $ unlines $ map ("- " <>) xs

-- | Validate that a key is present in a list of (projected) items
--
-- Returns the item found when validation passes.
--
-- N.B. the @[c]@ values are almost always going to be @'String'@. It's typed as
-- a list of @c@ because the only actual requirement is that there are equatable
-- elements so we can compute edit-distances.
--
validateExpectedKeyBy
    :: (Eq c, Show c)
    => String -- ^ The label to show as /Unknown \<label> .../
    -> (a -> [c])
    -- ^ A function to project each valid value as a comparable key
    -> [a] -- ^ The input list of valid items
    -> [c] -- ^ The input key
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

nearestElem :: Eq a => [a] -> [[a]] -> Maybe ([a], Int)
nearestElem x = minimumBy (compare `on` snd) . map (id &&& editDistance x)

-- | Calculate the edit-distance between two words
--
-- From <https://wiki.haskell.org/Edit_distance>; I do not understand it.
--
-- Lightly modified (linted and auto-formatted).
--
editDistance :: Eq a => [a] -> [a] -> Int
editDistance a b = Unsafe.last $ if lab == 0
    then mainDiag
    else if lab > 0 then lowers !! (lab - 1) else uppers !! (-1 - lab)
  where
    mainDiag = oneDiag a b (Unsafe.head uppers) (-1 : Unsafe.head lowers)
    uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
    lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
    eachDiag _ [] _ = []
    eachDiag _ _ [] = []
    eachDiag _ (_ : bs) (lastDiag : diags) =
        oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
        where nextDiag = Unsafe.head (Unsafe.tail diags)
    oneDiag _ _ diagAbove diagBelow = thisdiag
      where
        doDiag [] _ _ _ _ = []
        doDiag _ [] _ _ _ = []
        doDiag (ach : as) (bch : bs) nw n w = me
            : doDiag as bs me (Unsafe.tail n) (Unsafe.tail w)
          where
            me = if ach == bch
                then nw
                else 1 + min3 (Unsafe.head w) nw (Unsafe.head n)
        firstelt = 1 + Unsafe.head diagBelow
        thisdiag =
            firstelt : doDiag a b firstelt diagAbove (Unsafe.tail diagBelow)
    lab = length a - length b
    min3 x y z = if x < y then x else min y z
