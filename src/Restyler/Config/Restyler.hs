{-# LANGUAGE LambdaCase #-}

module Restyler.Config.Restyler
    ( RestylerOverride
    , overrideRestylers
    , legacyOverrideRestylers
    )
where

import Restyler.Prelude

import Data.Aeson hiding (Result(..))
import Data.Aeson.Casing
import Data.Aeson.Types (Parser, modifyFailure)
import qualified Data.HashMap.Lazy as HM
import Data.Validation
import Restyler.Config.ExpectedKeys
import Restyler.Config.Glob
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Config.SketchyList
import Restyler.Restyler

data RestylerOverride = RestylerOverride
    { roName :: String
    -- ^ TODO: Make this @Glob@ once the @restylers@ key is gone
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

overrideRestylers :: [Restyler] -> [RestylerOverride] -> [Restyler]
overrideRestylers restylers overrides =
    map (overrideRestyler overrides) restylers

overrideRestyler :: [RestylerOverride] -> Restyler -> Restyler
overrideRestyler overrides restyler@Restyler {..} = fromMaybe restyler $ do
    RestylerOverride {..} <- find (matchOverride restyler) overrides

    pure $ restyler
        { rEnabled = fromMaybe rEnabled roEnabled
        , rImage = fromMaybe rImage roImage
        , rCommand = maybe rCommand unSketchy roCommand
        , rArguments = maybe rArguments unSketchy roArguments
        , rInclude = maybe rInclude unSketchy roInclude
        , rInterpreters = maybe rInterpreters unSketchy roInterpreters
        }

matchOverride :: Restyler -> RestylerOverride -> Bool
matchOverride Restyler {..} RestylerOverride {..} = glob roName `match` rName

legacyOverrideRestylers
    :: [Restyler] -> [RestylerOverride] -> Either [String] [Restyler]
legacyOverrideRestylers restylers =
    toEither . legacyOverrideRestylers' restylers

-- | @'legacyOverrideRestylers'@ in @'Validation'@
legacyOverrideRestylers'
    :: [Restyler] -> [RestylerOverride] -> Validation [String] [Restyler]
legacyOverrideRestylers' restylers = traverse
    $ legacyOverrideRestyler restylersMap
  where
    restylersMap :: HashMap String Restyler
    restylersMap = HM.fromList $ map (rName &&& id) restylers

legacyOverrideRestyler
    :: HashMap String Restyler
    -> RestylerOverride
    -> Validation [String] Restyler
legacyOverrideRestyler restylers RestylerOverride {..} =
    override <$> lookupExpectedKeyBy "Restyler name" restylers roName
  where
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
