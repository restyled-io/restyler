{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-term-variable-capture #-}

-- |
--
-- Module      : Restyler.Config.Restyler
-- Copyright   : (c) 2025 Patrick Brisbin
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

    -- * Exported for testing
  , autoEnableOverrides
  , overrideRestylers
  ) where

import Restyler.Prelude hiding ((.=))

import Autodocodec
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Validation
import OptEnvConf hiding (name)
import Restyler.AnnotatedException (throw)
import Restyler.Config.AutoEnable
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
    [ help
        $ unpack
        $ unlines
          [ "Version of Restylers manifest to use"
          , "Ignored if manifest is given"
          ]
    , option
    , long "restylers-version"
    , reader str
    , metavar "TAG"
    , env "RESTYLERS_VERSION"
    , conf "restylers_version"
    , value "stable"
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

wildcard :: RestylerOverride
wildcard = restylerOverride "*"

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
  :: ( HasCallStack
     , HasManifest env
     , HasRestylerOverrides env
     , HasRestylersVersion env
     , MonadDirectory m
     , MonadDownloadFile m
     , MonadIO m
     , MonadLogger m
     , MonadReader env m
     )
  => m [Restyler]
getEnabledRestylers = do
  version <- asks getRestylersVersion
  restylers <- getAllRestylersVersioned version
  overrides <- asks getRestylerOverrides
  overrides' <- autoEnableOverrides restylers overrides
  either (throw . RestylersInvalid) (pure . filter rEnabled)
    $ overrideRestylers restylers overrides'

autoEnableOverrides
  :: (MonadDirectory m, MonadLogger m)
  => [Restyler]
  -> [RestylerOverride]
  -> m [RestylerOverride]
autoEnableOverrides restylers overrides = do
  results <- toAutoEnableResults explicitNames autoEnablePairs
  autoEnables <- concat <$> traverse resultToOverride results
  pure $ autoEnables <> overrides
 where
  explicitNames = map (.name) $ filter (fromMaybe True . (.enabled)) overrides
  autoEnablePairs = mapMaybe (\r -> (pack $ rName r,) <$> rAutoEnable r) restylers

resultToOverride :: MonadLogger m => AutoEnableResult -> m [RestylerOverride]
resultToOverride = \case
  AutoEnableSome msg names -> do
    logInfo $ "Auto-enable" :# ["restylers" Aeson..= names, "reason" Aeson..= msg]
    pure $ map autoEnableOverride $ toList names
  AutoEnableNone -> pure []

restylerOverridesParser :: Parser [RestylerOverride]
restylerOverridesParser =
  withShownDefault (const "[\"*\"]") [wildcard]
    $ setting
      [ help "Restylers to run, and how"
      , example
          $ unpack
          $ unlines
            [ "# Elements in this list can be specified in one of three forms:"
            , "# A string, which means to run that Restyler with all defaults"
            , "restylers:"
            , "  - prettier"
            ]
      , example
          $ unpack
          $ unlines
            [ "# A single key, that is a name, a Restyler (see Schema) as the"
            , "# value:"
            , "restylers:"
            , "  - prettier:"
            , "      include:"
            , "        - \"**/*.js\""
            ]
      , example
          $ unpack
          $ unlines
            [ "# Or a Restyler including a name key:"
            , "restylers:"
            , "  - name: prettier"
            , "    include:"
            , "      - \"**/*.js\""
            ]
      , example
          $ unpack
          $ unlines
            [ "# All three of the above are equivalent. The latter two are useful"
            , "# if you want to run the same Restyler multiple ways:"
            , "restylers:"
            , "  - name: prettier"
            , "    arguments: [\"--one-thing\"]"
            , "    include: [\"needs-one-thing/**/*.js\"]"
            , ""
            , "  - name: prettier"
            , "    arguments: [\"--another\"]"
            , "    include: [\"needs-another/**/*.js\"]"
            ]
      , example
          $ unpack
          $ unlines
            [ "# Omitted keys inherit defaults for the Restyler of that name."
            , "#"
            , "# Except the enabled key. Adding an item to this list without"
            , "# specifying {enabled:false}, automatically enables that Restyler."
            , "#"
            , "# In string form, prefixing the name with ! is short-hand for"
            , "# disabling. The following two configurations are equivalent:"
            , "restylers:"
            , "  - \"!astyle\" # quoting is required for this"
            , "  - astyle:"
            , "      enabled: false"
            ]
      , example
          $ unpack
          $ unlines
            [ "# The special value * (wildcard) means all Restylers not"
            , "# configured. One wildcard may be placed anywhere in the"
            , "# restylers list and remaining Restylers will be run, with their"
            , "# default values at that point."
            , "#"
            , "# Note that the Restylers added by the * entry will not run if"
            , "# they're default configuration includes {enabled:false}. You must"
            , "# explicitly add such Restylers for them to run."
            , "#"
            , "# Just run all Restylers with default values, i.e. the default"
            , "# configuration value:"
            , "restylers:"
            , "  - \"*\""
            ]
      , example
          $ unpack
          $ unlines
            [ "# Enable jdt, and run all others after"
            , "restylers:"
            , "  - jdt"
            , "  - \"*\""
            ]
      , example
          $ unpack
          $ unlines
            [ "# Enable jdt, and run it after all others"
            , "restylers:"
            , "  - \"*\""
            , "  - jdt"
            ]
      , example
          $ unpack
          $ unlines
            [ "# Ensure stylish-haskell runs before brittany, and before all others"
            , "restylers:"
            , "  - stylish-haskell"
            , "  - brittany"
            , "  - \"*\""
            ]
      , example
          $ unpack
          $ unlines
            [ "# Run only clang-format"
            , "restylers:"
            , "  - clang-format"
            ]
      , example
          $ unpack
          $ unlines
            [ "# Run clang-format, astyle, everything else, then clang-format again with different options"
            , "restylers:"
            , "  - clang-format"
            , "  - astyle"
            , "  - \"*\""
            , "  - clang-format:"
            , "      arguments: [\"--special\"]"
            , "      include:"
            , "        - \"special/**/*.cs\""
            ]
      , example
          $ unpack
          $ unlines
            [ "# Disable the astyle Restyler, maintaining all other defaults"
            , "restylers:"
            , "  - \"!astyle\""
            , "  - \"*\""
            ]
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

autoEnableOverride :: Text -> RestylerOverride
autoEnableOverride name =
  RestylerOverride
    { name
    , enabled = Just True
    , image = Nothing
    , command = Nothing
    , arguments = Nothing
    , include = Nothing
    , interpreters = Nothing
    , delimiters = Nothing
    }
