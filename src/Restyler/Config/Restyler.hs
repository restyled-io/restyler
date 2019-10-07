{-# LANGUAGE LambdaCase #-}

-- | The Restyler attributes that can be used in a "named override"
--
-- The @'FromJSON'@ for @'Restyler'@ is the "obvious" and literal parser, it is
-- used to read the static @'allRestylers'@ information. Once we have that data,
-- we're able to support a succincter config that gives the name of a known
-- Restyler and overrides some fields:
--
-- @
-- hlint:
--   arguments:
--     - foo
--     - bar
-- @
--
-- This module accomplishes that.
--
module Restyler.Config.Restyler
    ( RestylerOverrides
    , overrideRestylers
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types (Parser, typeMismatch)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Restyler.Config.ExpectedKeys
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Config.SketchyList
import Restyler.Restyler

data RestylerOverrides = RestylerOverrides
    { _roLegacy :: Bool
    , _roOverrides :: HashMap String RestylerOverride
    }

-- brittany-disable-next-binding

instance FromJSON RestylerOverrides where
    parseJSON = \case
        Object hm -> RestylerOverrides False <$> parseOverride hm
        Array v -> RestylerOverrides True <$> parseLegacyList v
        v -> typeMismatch "Restyler overrides object" v

parseOverride :: HashMap Text Value -> Parser (HashMap String RestylerOverride)
parseOverride = fmap HM.fromList . traverse parseOverrideElement . HM.toList

parseOverrideElement :: (Text, Value) -> Parser (String, RestylerOverride)
parseOverrideElement = bimapM (pure . unpack) parseJSON

data RestylerOverride = RestylerOverride
    { roEnabled :: Maybe Bool
    , roImage :: Maybe String
    , roCommand :: Maybe (SketchyList String)
    , roArguments :: Maybe (SketchyList String)
    , roInclude :: Maybe (SketchyList Include)
    , roInterpreters :: Maybe (SketchyList Interpreter)
    }
    deriving (Eq, Show, Generic)

instance FromJSON RestylerOverride where
    parseJSON = genericParseJSONValidated $ aesonPrefix snakeCase

defaultEnabled :: RestylerOverride -> RestylerOverride
defaultEnabled ro@RestylerOverride {..} =
    ro { roEnabled = roEnabled <|> Just True }

overrideRestylers :: [Restyler] -> RestylerOverrides -> Either String [Restyler]
overrideRestylers restylers (RestylerOverrides legacy hm) =
    map override restylers <$ validateRestylerNames
  where
    validateRestylerNames = traverse_ validateRestylerName $ HM.keys hm
    validateRestylerName =
        validateExpectedKeyBy "Restyler name" rName restylers

    override base =
        maybe (disabledIfLegacy base) (overrideRestyler base)
            $ HM.lookup (rName base) hm

    disabledIfLegacy r
        | legacy = r { rEnabled = False }
        | otherwise = r

overrideRestyler :: Restyler -> RestylerOverride -> Restyler
overrideRestyler restyler@Restyler {..} RestylerOverride {..} = restyler
    { rEnabled = fromMaybe rEnabled roEnabled
    , rImage = fromMaybe rImage roImage
    , rCommand = maybe rCommand unSketchy roCommand
    , rArguments = maybe rArguments unSketchy roArguments
    , rInclude = maybe rInclude unSketchy roInclude
    , rInterpreters = maybe rInterpreters unSketchy roInterpreters
    }

-- | Parse the legacy list-style format
--
-- NB. we don't preserve 100% of the behavior from before:
--
-- 1. Previously, you could put a whole @'Restyler'@ object as a list element
--
--    Now this is an (informative) error. If this is limitation is a problem for
--    users, and the use-case is reasonable, we'll extend @'RestylerOverride'@.
--
-- 2. Presence in the list implies @enabled:true@ (as before), but non-presence
--    no longer implies @enabled:false@
--
--    Users should migrate to the new-style config, but just adding
--    @enabled:false@ to a list element would be respected too.
--
parseLegacyList :: Vector Value -> Parser (HashMap String RestylerOverride)
parseLegacyList = fmap HM.fromList . traverse parseLegacyElement . V.toList

parseLegacyElement :: Value -> Parser (String, RestylerOverride)
parseLegacyElement = \case
    String name -> namedOverride name $ Object HM.empty
    Object o -> case HM.toList o of
        [(name, vo@(Object _))] -> namedOverride name vo

        _ -> invalidLegacy
    _ -> invalidLegacy

namedOverride :: Text -> Value -> Parser (String, RestylerOverride)
namedOverride name o = (unpack name, ) . defaultEnabled <$> parseJSON o

invalidLegacy :: Parser a
invalidLegacy = fail $ unlines
    [ "Invalid restylers value: must be a name or name with overrides"
    , ""
    , "You may be using a feature previously supported in that list-style"
    , "configuration format. Please adjust your configuration to the"
    , "new object-style format."
    , ""
    ]
