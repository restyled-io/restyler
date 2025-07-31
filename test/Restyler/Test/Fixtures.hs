-- |
--
-- Module      : Restyler.Test.Fixtures
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Test.Fixtures
  ( -- * 'Restyler'
    someRestyler

    -- * 'RestylerOverride'
  , wildcard
  , enabled
  , disabled
  ) where

import Restyler.Prelude

import Restyler.Config.Restyler (RestylerOverride (..), restylerOverride)
import Restyler.Restyler (Restyler (..), RestylerRunStyle (..))

someRestyler :: String -> Restyler
someRestyler name =
  Restyler
    { rEnabled = True
    , rAutoEnable = Nothing
    , rName = name
    , rImage = "restyled/restyler-" <> name <> ":v1.0.0"
    , rCommand = ["restyle"]
    , rDocumentation = []
    , rArguments = []
    , rInclude = ["**/*"]
    , rInterpreters = []
    , rDelimiters = Nothing
    , rRunStyle = RestylerRunStylePathsOverwriteSep
    }

wildcard :: RestylerOverride
wildcard = restylerOverride "*"

enabled :: Text -> RestylerOverride
enabled name = (restylerOverride name) {enabled = Just True}

disabled :: Text -> RestylerOverride
disabled name = (restylerOverride name) {enabled = Just False}
