module Restyler.Config.Enabled
  ( HasEnabled (..)
  , enabledParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasEnabled env where
  getEnabled :: env -> Bool

enabledParser :: Parser Bool
enabledParser =
  setting
    [ help "Do anything at all"
    , conf "enabled"
    ]
