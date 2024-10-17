module Restyler.Options.Restrictions.Bytes
  ( Bytes (..)
  , readBytes
  , showBytes
  , Suffix (..)
  ) where

import Restyler.Prelude hiding ((.=))

import Autodocodec
import Data.Char qualified as Char

data Bytes = Bytes
  { number :: Natural
  , suffix :: Maybe Suffix
  }
  deriving stock (Generic, Eq, Show)

instance HasCodec Bytes where
  codec =
    object "Bytes"
      $ Bytes
      <$> (requiredField "number" "a number" .= (.number))
      <*> (optionalField "suffix" "a suffix" .= (.suffix))

readBytes :: String -> Either String Bytes
readBytes x =
  Bytes
    <$> readNat number
    <*> traverse
      readSuffix
      (guarded (not . null) suffix)
 where
  (number, suffix) = span ((||) <$> (== '-') <*> Char.isDigit) x

showBytes :: Bytes -> String
showBytes b = show b.number <> maybe "" showSuffix b.suffix

data Suffix = B | K | M | G
  deriving stock (Bounded, Enum, Eq, Show)

instance HasCodec Suffix where
  codec = boundedEnumCodec $ pack . showSuffix

readSuffix :: String -> Either String Suffix
readSuffix = \case
  "b" -> Right B
  "k" -> Right K
  "m" -> Right M
  "g" -> Right G
  x -> Left $ "Invalid suffix " <> x <> ", must be one of b, k, m, or g"

showSuffix :: Suffix -> String
showSuffix = \case
  B -> "b"
  K -> "k"
  M -> "m"
  G -> "g"