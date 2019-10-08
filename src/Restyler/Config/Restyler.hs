{-# LANGUAGE LambdaCase #-}

module Restyler.Config.Restyler
    ( RestylerOverride
    , overrideRestylers
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types (Parser, modifyFailure)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
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
    parseJSON
        . Object
        . HM.insertWith const "name" (String name)
        . HM.delete name

suffixIncorrectIndentation :: Parser a -> Parser a
suffixIncorrectIndentation = modifyFailure (<> msg)
    where msg = "\n\nDo you have incorrect indentation for a named override?"

overrideRestylers
    :: [Restyler] -> Maybe [RestylerOverride] -> Either String [Restyler]
overrideRestylers restylers = maybe (pure restylers)
    $ traverse (overrideRestyler restylersMap)
  where
    restylersMap :: HashMap String Restyler
    restylersMap = HM.fromList $ map (rName &&& id) restylers

overrideRestyler
    :: HashMap String Restyler -> RestylerOverride -> Either String Restyler
overrideRestyler restylers RestylerOverride {..} = do
    restyler@Restyler {..} <- lookupExpectedKeyBy
        "Restyler name"
        restylers
        roName

    pure restyler
        { rEnabled = fromMaybe True roEnabled
        , rImage = fromMaybe rImage roImage
        , rCommand = maybe rCommand unSketchy roCommand
        , rArguments = maybe rArguments unSketchy roArguments
        , rInclude = maybe rInclude unSketchy roInclude
        , rInterpreters = maybe rInterpreters unSketchy roInterpreters
        }
