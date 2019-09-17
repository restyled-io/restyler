{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Function for getting the names of fields in a type
--
-- <https://stackoverflow.com/questions/27815489/is-it-possible-to-list-the-names-and-types-of-fields-in-a-record-data-type-that>
--
module GHC.Generics.Selectors
    ( Selectors(..)
    )
where

import Prelude

import Data.Proxy
import GHC.Generics

class Selectors rep where
  selectors :: Proxy rep -> [String]

instance Selectors f => Selectors (M1 D x f) where
    selectors _ = selectors (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
    selectors _ = selectors (Proxy :: Proxy f)

instance Selector s => Selectors (M1 S s (K1 R t)) where
    selectors _ = [selName (undefined :: M1 S s (K1 R t) ())]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
    selectors _ = selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b)

instance Selectors U1 where
    selectors _ = []
