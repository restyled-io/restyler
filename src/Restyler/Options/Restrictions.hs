-- |
--
-- Module      : Restyler.Options.Restrictions
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.Restrictions
  ( HasRestrictions (..)
  , Restrictions (..)
  , restrictionsParser
  , restrictionOptions
  ) where

import Restyler.Prelude

import Data.Semigroup.Generic
import OptEnvConf
import Restyler.Options.Restrictions.Bytes

class HasRestrictions a where
  getRestrictions :: a -> Restrictions

data Restrictions = Restrictions
  { netNone :: Any
  , cpuShares :: Last Natural
  , memory :: Last Bytes
  }
  deriving stock (Generic, Eq, Show)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid Restrictions

restrictionOptions :: Restrictions -> [String]
restrictionOptions r =
  concat
    [ ["--net=none" | getAny r.netNone]
    , ["--cpu-shares=" <> show s | s <- maybeToList $ getLast r.cpuShares]
    , ["--memory=" <> showBytes b | b <- maybeToList $ getLast r.memory]
    ]

restrictionsParser :: Parser Restrictions
restrictionsParser =
  (<>)
    <$> unrestrictedParser
    <*> subRestrictionsParser

unrestrictedParser :: Parser Restrictions
unrestrictedParser =
  bool noRestrictions fullRestrictions
    <$> yesNoSwitch
      [ help "Restrict restylers resources"
      , long "restricted"
      , env "RESTRICTED"
      , conf "restricted"
      , value True
      ]

subRestrictionsParser :: Parser Restrictions
subRestrictionsParser =
  subAll "restyler"
    $ Restrictions
    <$> ( Any
            <$> yesNoSwitch
              [ help "Run restylers with --net=none"
              , long "net-none"
              , env "NET_NONE"
              , conf "net_none"
              , value True
              ]
        )
    <*> ( Last
            <$> optional
              ( setting
                  [ help "Run restylers with --cpu-shares"
                  , option
                  , long "cpu-shares"
                  , env "CPU_SHARES"
                  , metavar "NUMBER"
                  , conf "cpu_shares"
                  , reader $ eitherReader readNat
                  ]
              )
        )
    <*> ( Last
            <$> optional
              ( setting
                  [ help "Run restylers with --memory"
                  , option
                  , long "memory"
                  , env "MEMORY"
                  , conf "memory"
                  , metavar "NUMBER<b|k|m|g>"
                  , reader $ eitherReader readBytes
                  ]
              )
        )

noRestrictions :: Restrictions
noRestrictions =
  Restrictions
    { netNone = Any False
    , cpuShares = Last Nothing
    , memory = Last Nothing
    }

fullRestrictions :: Restrictions
fullRestrictions =
  Restrictions
    { netNone = Any True
    , cpuShares = Last $ Just defaultCpuShares
    , memory = Last $ Just defaultMemory
    }

defaultCpuShares :: Natural
defaultCpuShares = 128

defaultMemory :: Bytes
defaultMemory =
  Bytes
    { number = 512
    , suffix = Just M
    }
