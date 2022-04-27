module Restyler.Config.Restyler
    ( RestylerOverride
    , overrideRestylers
    ) where

import Restyler.Prelude

import Data.Aeson hiding (Result(..))
import Data.Aeson.Casing
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, modifyFailure)
import Restyler.Config.ExpectedKeys
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Config.SketchyList
import Restyler.Config.WildCard
import Restyler.Delimited
import Restyler.Restyler

data RestylerOverride = RestylerOverride
    { roName :: String
    , roEnabled :: Maybe Bool
    , roImage :: Maybe String
    , roCommand :: Maybe (SketchyList String)
    , roArguments :: Maybe (SketchyList String)
    , roInclude :: Maybe (SketchyList Include)
    , roInterpreters :: Maybe (SketchyList Interpreter)
    , roDelimiters :: Maybe Delimiters
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON RestylerOverride where
    parseJSON = \case
        String name -> namedOverride (Key.fromText name) KeyMap.empty
        Object o | [(name, Object o')] <- KeyMap.toList o ->
            namedOverride name o'
        v -> suffixIncorrectIndentation
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
    :: [Restyler] -> [WildCard RestylerOverride] -> Either [String] [Restyler]
overrideRestylers =
    overrideWildCard "Restyler name" rName roName overrideRestyler

overrideRestyler :: Restyler -> RestylerOverride -> Restyler
overrideRestyler restyler@Restyler {..} RestylerOverride {..} = restyler
    { rEnabled = fromMaybe True roEnabled
    , rImage = fromMaybe rImage roImage
    , rCommand = maybe rCommand unSketchy roCommand
    , rArguments = maybe rArguments unSketchy roArguments
    , rInclude = maybe rInclude unSketchy roInclude
    , rInterpreters = maybe rInterpreters unSketchy roInterpreters
    , rDelimiters = roDelimiters <|> rDelimiters
    }
