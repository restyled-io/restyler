-- | The Restyler attributes that can be used in a "named override"
--
-- The @'FromJSON'@ for @'Restyler'@ is the "obvious" and literal parser, it is
-- used to read the static @'allRestylers'@ information. Once we have that data,
-- we're able to support a succincter config that gives the name of a known
-- Restyler and overrides some fields:
--
-- @
-- - hlint:
--     arguments:
--       - foo
--       - bar
-- @
--
-- This module accomplishes that.
--
module Restyler.Config.Restyler
    ( ConfigRestyler
    , unConfigRestyler
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types (Parser, modifyFailure, typeMismatch)
import qualified Data.HashMap.Lazy as HM
import Restyler.Config.ExpectedKeys
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Config.SketchyList
import Restyler.Restyler

data RestylerOverride = RestylerOverride
    { roImage :: Maybe String
    , roCommand :: Maybe (SketchyList String)
    , roArguments :: Maybe (SketchyList String)
    , roInclude :: Maybe (SketchyList Include)
    , roInterpreters :: Maybe (SketchyList Interpreter)
    }
    deriving (Eq, Show, Generic)

overrideRestyler :: Restyler -> RestylerOverride -> Restyler
overrideRestyler restyler@Restyler {..} RestylerOverride {..} = restyler
    { rImage = fromMaybe rImage roImage
    , rCommand = maybe rCommand unSketchy roCommand
    , rArguments = maybe rArguments unSketchy roArguments
    , rInclude = maybe rInclude unSketchy roInclude
    , rInterpreters = maybe rInterpreters unSketchy roInterpreters
    }

instance FromJSON RestylerOverride where
    parseJSON = genericParseJSONValidated $ aesonPrefix snakeCase

newtype ConfigRestyler = ConfigRestyler
    { unConfigRestyler :: [Restyler] -> Either String Restyler
    }

configRestyler :: Text -> RestylerOverride -> ConfigRestyler
configRestyler name override = ConfigRestyler $ \restylers -> do
    restyler <- lookupRestyler name restylers
    pure $ overrideRestyler restyler override

-- brittany-disable-next-binding

instance FromJSON ConfigRestyler where
    parseJSON (String name) = pure $ ConfigRestyler $ lookupRestyler name
    parseJSON v@(Object o) = case HM.toList o of
        [(name, vo@(Object _))] -> configRestyler name <$> parseJSON vo
        _ -> suffixFailure
            "\n\nDid you intend to specify a full Restyler object, or do you have incorrect indentation for a named override?"
            $ ConfigRestyler . const . Right <$> parseJSON v
    parseJSON v = typeMismatch "Name, or name with overrides" v

suffixFailure :: String -> Parser a -> Parser a
suffixFailure x = modifyFailure (<> x)

lookupRestyler :: Text -> [Restyler] -> Either String Restyler
lookupRestyler name restylers =
    validateExpectedKeyBy "Restyler name" rName restylers $ unpack name
