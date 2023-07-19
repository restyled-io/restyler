module Restyler.Config.Restyler
  ( RestylerOverride
  , overrideRestylers
  ) where

import Restyler.Prelude

import Data.Aeson hiding (Result (..))
import Data.Aeson.Casing
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, modifyFailure)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Validation
import Restyler.Config.ExpectedKeys
import Restyler.Config.Image
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Config.SketchyList
import Restyler.Delimited
import Restyler.Restyler

data RestylerOverride = RestylerOverride
  { roName :: String
  , roEnabled :: Maybe Bool
  , roImage :: Maybe Image
  , roCommand :: Maybe (SketchyList String)
  , roArguments :: Maybe (SketchyList String)
  , roInclude :: Maybe (SketchyList Include)
  , roInterpreters :: Maybe (SketchyList Interpreter)
  , roDelimiters :: Maybe Delimiters
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON RestylerOverride where
  parseJSON = \case
    String name'
      | Just name <- T.stripPrefix "!" name' ->
          namedOverride (Key.fromText name)
            $ KeyMap.singleton "enabled" (Bool False)
    String name -> namedOverride (Key.fromText name) KeyMap.empty
    Object o
      | [(name, Object o')] <- KeyMap.toList o ->
          namedOverride name o'
    v ->
      suffixIncorrectIndentation
        $ genericParseJSONValidated (aesonPrefix snakeCase) v

namedOverride :: Key -> KeyMap Value -> Parser RestylerOverride
namedOverride name =
  parseJSON
    . Object
    . insertIfMissing "name" (String $ Key.toText name)
    . KeyMap.delete name

suffixIncorrectIndentation :: Parser a -> Parser a
suffixIncorrectIndentation = modifyFailure (<> msg)
 where
  msg :: String
  msg = "\n\nDo you have incorrect indentation for a named override?"

overrideRestylers
  :: [Restyler] -> [RestylerOverride] -> Either [Text] [Restyler]
overrideRestylers restylers overrides =
  toEither $ case length $ filter ((== "*") . roName) overrides of
    0 -> explicits <$> getOverrides
    1 -> replaced restylers <$> getOverrides
    n ->
      Failure
        [ "You may have at most 1 wildcard in restylers ("
            <> show n
            <> " found)"
        ]
 where
  getOverrides = traverse (overrideRestyler restylersMap) overrides

  restylersMap :: HashMap Text Restyler
  restylersMap = HashMap.fromList $ map (pack . rName &&& id) restylers

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
  :: HashMap Text Restyler -> RestylerOverride -> Validation [Text] Override
overrideRestyler restylers RestylerOverride {..}
  | roName == "*" = pure Wildcard
  | otherwise = Explicit . override <$> defaults
 where
  defaults = lookupExpectedKeyBy "Restyler name" restylers $ pack roName
  override restyler@Restyler {..} =
    restyler
      { rEnabled = fromMaybe True roEnabled
      , rImage = maybe rImage (overrideRestylerImage rImage) roImage
      , rCommand = maybe rCommand unSketchy roCommand
      , rArguments = maybe rArguments unSketchy roArguments
      , rInclude = maybe rInclude unSketchy roInclude
      , rInterpreters = maybe rInterpreters unSketchy roInterpreters
      , rDelimiters = roDelimiters <|> rDelimiters
      }

lookupExpectedKeyBy :: Text -> HashMap Text v -> Text -> Validation [Text] v
lookupExpectedKeyBy label hm k =
  case validateExpectedKeyBy label fst (HashMap.toList hm) k of
    Left e -> Failure [e]
    Right (_k, v) -> Success v
