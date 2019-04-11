{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelper
    ( module X
    )
where

import Restyler.Prelude as X
import Test.Hspec as X
import Test.QuickCheck as X

import GitHub.Data (Id, Name, mkId, mkName)

instance Num (Id a) where
    -- Just so we can type literals for Ids in Specs
    fromInteger = mkId Proxy . fromInteger

    (+) = error "NO"
    (-) = error "NO"
    (*) = error "NO"
    abs = error "NO"
    signum = error "NO"

instance Arbitrary (Name a) where
    arbitrary = mkName Proxy . pack <$> arbitrary `suchThat` all goodChar
      where
        goodChar c
            | isSpace c = False
            | otherwise = c `notElem` ['/', '#']
