{-# LANGUAGE DerivingVia #-}

module Restyler.Restrictions
  ( HasRestrictions (..)
  , Restrictions (..)
  , restrictionOptions
  , envRestrictions
  , fullRestrictions

    -- * Bytes
  , Bytes (..)
  , Suffix (..)
  , bytesOption
  , readBytes
  ) where

import Restyler.Prelude

import Data.Char qualified as Char
import Data.Semigroup.Generic
import Env qualified

class HasRestrictions env where
  restrictionsL :: Lens' env Restrictions

data Restrictions = Restrictions
  { netNone :: Last Bool
  , capDropAll :: Last Bool
  , cpuShares :: Last Natural
  , memory :: Last Bytes
  }
  deriving stock (Generic, Eq, Show)
  deriving (Semigroup) via GenericSemigroupMonoid Restrictions

restrictionOptions :: Restrictions -> [String]
restrictionOptions Restrictions {..} =
  concat
    $ catMaybes
      [ (\b -> if b then ["--net", "none"] else []) <$> getLast netNone
      , (\b -> if b then ["--cap-drop", "all"] else []) <$> getLast capDropAll
      , (\n -> ["--cpu-shares", show n]) <$> getLast cpuShares
      , (\bs -> ["--memory", bytesOption bs]) <$> getLast memory
      ]

envRestrictions :: Env.Parser Env.Error Restrictions
envRestrictions =
  (<>)
    <$> Env.flag
      fullRestrictions
      noRestrictions
      "UNRESTRICTED"
      (Env.help "Run restylers without CPU or Memory restrictions")
    <*> parseOverrides

parseOverrides :: Env.Parser Env.Error Restrictions
parseOverrides =
  Env.prefixed "RESTYLER_"
    $ Restrictions
    <$> ( fmap not
            <$> lastSwitch
              "NO_NET_NONE"
              "Run restylers without --net=none"
        )
    <*> ( fmap not
            <$> lastSwitch
              "NO_CAP_DROP_ALL"
              "Run restylers without --cap-drop=all"
        )
    <*> lastReader
      readNat
      "CPU_SHARES"
      "Run restylers with --cpu-shares=<number>"
    <*> lastReader
      readBytes
      "MEMORY"
      "Run restylers with --memory=<number>[b|k|m|g]"
 where
  lastSwitch
    :: String
    -> String
    -> Env.Parser Env.Error (Last Bool)
  lastSwitch name h =
    Last <$> Env.flag Nothing (Just True) name (Env.help h)

  lastReader
    :: (String -> Either String a)
    -> String
    -> String
    -> Env.Parser Env.Error (Last a)
  lastReader r name h =
    Last
      <$> Env.var
        (bimap Env.UnreadError Just . r)
        name
        (Env.def Nothing <> Env.help h)

fullRestrictions :: Restrictions
fullRestrictions =
  Restrictions
    { netNone = Last $ Just True
    , capDropAll = Last $ Just True
    , cpuShares = Last $ Just defaultCpuShares
    , memory = Last $ Just defaultMemory
    }

defaultCpuShares :: Natural
defaultCpuShares = 128

defaultMemory :: Bytes
defaultMemory = Bytes {bytesNumber = 512, bytesSuffix = Just M}

noRestrictions :: Restrictions
noRestrictions =
  Restrictions
    { netNone = Last $ Just False
    , capDropAll = Last $ Just False
    , cpuShares = Last Nothing
    , memory = Last Nothing
    }

data Bytes = Bytes
  { bytesNumber :: Natural
  , bytesSuffix :: Maybe Suffix
  }
  deriving stock (Eq, Show)

data Suffix = B | K | M | G
  deriving stock (Eq, Show)

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

bytesOption :: Bytes -> String
bytesOption Bytes {..} = show bytesNumber <> maybe "" showSuffix bytesSuffix

readBytes :: String -> Either String Bytes
readBytes x =
  Bytes
    <$> readNat number
    <*> traverse
      readSuffix
      (guarded (not . null) suffix)
 where
  (number, suffix) = span ((||) <$> (== '-') <*> Char.isDigit) x

readNat :: String -> Either String Natural
readNat n = first (const $ "Not a valid natural number: " <> n) (readEither n)
