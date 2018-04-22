{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelper
    ( module X
    ) where

import ClassyPrelude as X
import Test.Hspec as X

import Data.Proxy
import GitHub.Data (Id, mkId)

instance Num (Id a) where
    -- Just so we can type literals for Ids in Specs
    fromInteger = mkId Proxy . fromInteger

    (+) = error "NO"
    (-) = error "NO"
    (*) = error "NO"
    abs = error "NO"
    signum = error "NO"
