-- |
--
-- Module      : Restyler.Config.Restrictions
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Restrictions
  ( HasRestrictions (..)
  , Restrictions (..)
  , restrictionsParser
  , restrictionOptions
  ) where

import Restyler.Prelude

import OptEnvConf
import Restyler.Config.Restrictions.Bytes

class HasRestrictions a where
  getRestrictions :: a -> Restrictions

data Restrictions = Restrictions
  { netNone :: Bool
  , cpuShares :: Maybe Natural
  , memory :: Maybe Bytes
  }
  deriving stock (Eq, Show)

restrictionOptions :: Restrictions -> [String]
restrictionOptions r =
  concat
    [ ["--net=none" | r.netNone]
    , ["--cpu-shares=" <> show s | s <- maybeToList r.cpuShares]
    , ["--memory=" <> showBytes b | b <- maybeToList r.memory]
    ]

restrictionsParser :: Parser Restrictions
restrictionsParser =
  go
    <$> subRestrictionsParser
    <*> restrictedParser
 where
  go :: Restrictions -> Bool -> Restrictions
  go rs = \case
    True -> rs
    False ->
      Restrictions
        { netNone = False
        , cpuShares = Nothing
        , memory = Nothing
        }

subRestrictionsParser :: Parser Restrictions
subRestrictionsParser =
  Restrictions
    <$> withDefault
      True
      ( yesNoSwitch
          [ help "Run restylers with --net=none"
          , long "net-none"
          , env "NET_NONE"
          , conf "net_none"
          ]
      )
    <*> ( Just
            <$> setting
              [ help "Run restylers with --cpu-shares"
              , option
              , long "cpu-shares"
              , env "CPU_SHARES"
              , metavar "NUMBER"
              , conf "cpu_shares"
              , reader $ eitherReader readNat
              , value 512
              ]
        )
    <*> ( Just
            <$> withShownDefault
              showBytes
              (Bytes 128 $ Just M)
              ( setting
                  [ help "Run restylers with --memory"
                  , option
                  , name "memory"
                  , metavar "NUMBER<b|k|m|g>"
                  , reader $ eitherReader readBytes
                  ]
              )
        )

restrictedParser :: Parser Bool
restrictedParser =
  withDefault True
    $ yesNoSwitch
      [ help "Restrict restylers resources"
      , name "restricted"
      ]
