{-# LANGUAGE LambdaCase #-}

module Restyler.Config.Restyler
    ( RestylerOverride
    , overrideRestylers
    )
where

import Restyler.Prelude

import Data.Aeson hiding (Result(..))
import Data.Aeson.Casing
import Data.Aeson.Types (Parser, modifyFailure)
import qualified Data.HashMap.Lazy as HM
import Data.Validation
import Restyler.Config.ExpectedKeys
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Config.SketchyList
import Restyler.Restyler

data RestylerOverride = RestylerOverride
    { roName :: String
    , roEnabled :: Maybe Bool
    , roImage :: Maybe String
    , roCommand :: Maybe (SketchyList String)
    , roArguments :: Maybe (SketchyList String)
    , roInclude :: Maybe (SketchyList Include)
    , roInterpreters :: Maybe (SketchyList Interpreter)
    }
    deriving (Eq, Show, Generic)

instance FromJSON RestylerOverride where
    parseJSON = \case
        String name -> namedOverride name HM.empty
        Object o | [(name, Object o')] <- HM.toList o -> namedOverride name o'
        v -> suffixIncorrectIndentation
            $ genericParseJSONValidated (aesonPrefix snakeCase) v

namedOverride :: Text -> HashMap Text Value -> Parser RestylerOverride
namedOverride name =
    parseJSON . Object . insertIfMissing "name" (String name) . HM.delete name

suffixIncorrectIndentation :: Parser a -> Parser a
suffixIncorrectIndentation = modifyFailure (<> msg)
    where msg = "\n\nDo you have incorrect indentation for a named override?"

overrideRestylers
    :: [Restyler] -> [RestylerOverride] -> Either [String] [Restyler]
overrideRestylers restylers overrides =
    toEither $ case length $ filter ((== "*") . roName) overrides of
        0 -> explicits <$> getOverrides
        1 -> replaced restylers <$> getOverrides
        n -> Failure
            [ "You may have at most 1 wildcard in restylers ("
              <> show n
              <> " found)"
            ]
  where
    getOverrides = traverse (overrideRestyler restylersMap) overrides

    restylersMap :: HashMap String Restyler
    restylersMap = HM.fromList $ map (rName &&& id) restylers

data Override = Explicit Restyler | Wildcard

explicits :: [Override] -> [Restyler]
explicits = concatMap $ \case
    Explicit r -> [r]
    Wildcard -> []

replaced :: [Restyler] -> [Override] -> [Restyler]
replaced restylers overrides = replaceWildcards others overrides
  where
    others = filter ((`notElem` overriden) . rName) restylers
    overriden = map rName $ explicits overrides

replaceWildcards :: [Restyler] -> [Override] -> [Restyler]
replaceWildcards restylers = concatMap $ \case
    Explicit r -> [r]
    Wildcard -> restylers

overrideRestyler
    :: HashMap String Restyler
    -> RestylerOverride
    -> Validation [String] Override
overrideRestyler restylers RestylerOverride {..}
    | roName == "*" = pure Wildcard
    | otherwise = Explicit . override <$> defaults
  where
    defaults = lookupExpectedKeyBy "Restyler name" restylers roName
    override restyler@Restyler {..} = restyler
        { rEnabled = fromMaybe True roEnabled
        , rImage = fromMaybe rImage roImage
        , rCommand = maybe rCommand unSketchy roCommand
        , rArguments = maybe rArguments unSketchy roArguments
        , rInclude = maybe rInclude unSketchy roInclude
        , rInterpreters = maybe rInterpreters unSketchy roInterpreters
        }

lookupExpectedKeyBy
    :: String -> HashMap String v -> String -> Validation [String] v
lookupExpectedKeyBy label hm k =
    case validateExpectedKeyBy label fst (HM.toList hm) k of
        Left e -> Failure [e]
        Right (_k, v) -> Success v
