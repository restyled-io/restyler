{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelper
    ( module X
    )
where

import RIO as X hiding (first)

import Data.Bifunctor as X (first)
import Test.Hspec as X

import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text.Arbitrary ()
import GitHub.Data (Id, Name, mkId, mkName)
import Test.QuickCheck

instance Num (Id a) where
    -- Just so we can type literals for Ids in Specs
    fromInteger = mkId Proxy . fromInteger

    (+) = error "NO"
    (-) = error "NO"
    (*) = error "NO"
    abs = error "NO"
    signum = error "NO"

instance Arbitrary (Name a) where
    arbitrary = mkName Proxy <$> arbitrary `suchThat` T.all goodChar
      where
        goodChar c
            | isSpace c = False
            | otherwise = c `notElem` ['/', '#']
