{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelper
    ( module X
    )
where

import Restyler.Prelude as X
import Test.Hspec as X
import Test.QuickCheck as X

deriving instance Num IssueNumber
deriving instance Arbitrary IssueNumber

instance Arbitrary (Name a) where
    arbitrary = mkName Proxy . pack <$> arbitrary `suchThat` all goodChar
      where
        goodChar c
            | isSpace c = False
            | otherwise = c `notElem` ['/', '#']
