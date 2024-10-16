-- |
--
-- Module      : Restyler.Config.Restyler
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Restyler
  ( HasRestylersVersion (..)
  , restylersVersionParser
  , HasRestylerOverrides (..)
  , RestylerOverride (..)
  , restylerOverride
  , restylerOverridesParser
  , RestylersInvalid (..)
  , getEnabledRestylers

    -- * To test
  , overrideRestylers
  ) where

import Restyler.Prelude hiding ((.=))

import Autodocodec
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Validation
import OptEnvConf hiding (name)
import Restyler.AnnotatedException (throw)
import Restyler.Config.Image
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Config.Manifest
import Restyler.Delimited
import Restyler.Monad.Directory
import Restyler.Monad.DownloadFile
import Restyler.Restyler

class HasRestylersVersion env where
  getRestylersVersion :: env -> String

restylersVersionParser :: Parser String
restylersVersionParser =
  setting
    [ help "Version of Restylers manifest to use"
    , option
    , long "restylers-version"
    , reader str
    , metavar "stable|dev|..."
    , env "RESTYLERS_VERSION"
    , conf "restylers_version"
    ]

class HasRestylerOverrides env where
  getRestylerOverrides :: env -> [RestylerOverride]

newtype RestylersInvalid = RestylersInvalid [Text]
  deriving stock (Show)
  deriving anyclass (Exception)

data RestylerOverride = RestylerOverride
  { name :: Text
  , enabled :: Maybe Bool
  , image :: Maybe Image
  , command :: Maybe [String]
  , arguments :: Maybe [String]
  , include :: Maybe [Include]
  , interpreters :: Maybe [Interpreter]
  , delimiters :: Maybe Delimiters
  }
  deriving stock (Eq, Show)

instance HasCodec RestylerOverride where
  codec =
    parseAlternatives
      codecObject
      [ codecNamed
      , codecNameOnly
      ]

restylerOverride :: Text -> RestylerOverride
restylerOverride name =
  RestylerOverride
    { name
    , enabled = Nothing
    , image = Nothing
    , command = Nothing
    , arguments = Nothing
    , include = Nothing
    , interpreters = Nothing
    , delimiters = Nothing
    }

-- | Parse or render the object normally
--
-- - @{...} -> ...@
codecObject :: JSONCodec RestylerOverride
codecObject =
  object "Restyler"
    $ RestylerOverride
    <$> (requiredField' "name" .= (.name))
    <*> (optionalField' "enabled" .= (.enabled))
    <*> (optionalField' "image" .= (.image))
    <*> (optionalField' "command" .= (.command))
    <*> (optionalField' "arguments" .= (.arguments))
    <*> (optionalField' "include" .= (.include))
    <*> (optionalField' "interpreters" .= (.interpreters))
    <*> (optionalField' "delimiters" .= (.delimiters))

-- | Parse a name key and sub-object. Not used for rendering
--
-- - @{name: {...}}" -> merge({name: name}, {...})@
codecNamed :: JSONCodec RestylerOverride
codecNamed =
  bimapCodec
    ( \case
        Object km | [(name, Object os)] <- KeyMap.toList km -> do
          parseEither (parseJSONVia codecObject)
            $ Object
            $ KeyMap.insert "name" (String $ Key.toText name) os
        _ -> Left "Not an Object"
    )
    (const "unused")
    valueCodec
    <?> "{name: Restyler}"

-- | Parse name-only syntax. Not used for rendering
--
-- - @"!<name>" -> {name: name, enabled:false}@
-- - @"<name>" -> {name: name, enabled:true}@
codecNameOnly :: JSONCodec RestylerOverride
codecNameOnly =
  bimapCodec
    ( \name -> Right $ case T.uncons name of
        Just (c, rest) | c == '!' -> (restylerOverride rest) {enabled = Just False}
        _ -> (restylerOverride name) {enabled = Just True}
    )
    ( \override ->
        -- This isn't totally correct, but we never render via this codec
        if override.enabled == Just False
          then "!" <> override.name
          else override.name
    )
    textCodec
    <?> "!<name>|<name>"

-- |
--
-- May throw 'RestylersInvalid'
getEnabledRestylers
  :: ( MonadIO m
     , MonadDirectory m
     , MonadDownloadFile m
     , MonadReader env m
     , HasRestylersVersion env
     , HasRestylerOverrides env
     , HasManifest env
     , HasCallStack
     )
  => m [Restyler]
getEnabledRestylers = do
  version <- asks getRestylersVersion
  overrides <- asks getRestylerOverrides
  restylers <- getAllRestylersVersioned version
  either
    (throw . RestylersInvalid)
    (pure . filter rEnabled)
    $ overrideRestylers restylers overrides

restylerOverridesParser :: Parser [RestylerOverride]
restylerOverridesParser =
  setting
    [ help "Restylers to run"
    , conf "restylers"
    ]

overrideRestylers
  :: [Restyler] -> [RestylerOverride] -> Either [Text] [Restyler]
overrideRestylers restylers overrides =
  toEither $ case length $ filter ((== "*") . (.name)) overrides of
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
overrideRestyler restylers o
  | o.name == "*" = pure Wildcard
  | otherwise = Explicit . override <$> defaults
 where
  defaults =
    case HashMap.lookup (o.name) restylers of
      Nothing -> Failure ["Unexpected Restyler name " <> o.name]
      Just v -> Success v

  override restyler =
    restyler
      { rEnabled = fromMaybe True o.enabled
      , rImage = maybe restyler.rImage (overrideRestylerImage restyler.rImage) o.image
      , rCommand = fromMaybe restyler.rCommand o.command
      , rArguments = fromMaybe restyler.rArguments o.arguments
      , rInclude = fromMaybe restyler.rInclude o.include
      , rInterpreters = fromMaybe restyler.rInterpreters o.interpreters
      , rDelimiters = o.delimiters <|> restyler.rDelimiters
      }
