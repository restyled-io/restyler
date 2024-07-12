module Restyler.HostDirectoryOption
  ( HostDirectoryOption (..)
  , HasHostDirectoryOption (..)
  , toHostDirectoryOption
  , unHostDirectoryOption
  ) where

import Restyler.Prelude

newtype HostDirectoryOption = HostDirectoryOption (Last FilePath)
  deriving newtype (Semigroup, Monoid)

class HasHostDirectoryOption env where
  hostDirectoryOptionL :: Lens' env HostDirectoryOption

toHostDirectoryOption :: Maybe FilePath -> HostDirectoryOption
toHostDirectoryOption = HostDirectoryOption . Last

unHostDirectoryOption :: HostDirectoryOption -> Maybe FilePath
unHostDirectoryOption (HostDirectoryOption x) = getLast x
