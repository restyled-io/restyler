{-# LANGUAGE FieldSelectors #-}

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
  , HasRestylerOverrides (..)
  , RestylerOverride
  , restylerOverridesParser
  , getEnabledRestylers
  ) where

import Restyler.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Validation
import OptEnvConf
import Restyler.Config.Image
import Restyler.Config.Include
import Restyler.Config.Interpreter
import Restyler.Config.SketchyList
import Restyler.Delimited
import Restyler.Monad.Directory
import Restyler.Monad.DownloadFile
import Restyler.Options.Manifest
import Restyler.Restyler

class HasRestylersVersion env where
  getRestylersVersion :: env -> String

class HasRestylerOverrides env where
  getRestylerOverrides :: env -> [RestylerOverride]

getEnabledRestylers
  :: ( MonadIO m
     , MonadDirectory m
     , MonadDownloadFile m
     , MonadReader env m
     , HasRestylersVersion env
     , HasRestylerOverrides env
     , HasManifest env
     )
  => m [Restyler]
getEnabledRestylers = do
  version <- asks getRestylersVersion
  restylers <- getAllRestylersVersioned version
  overrides <- asks getRestylerOverrides
  case overrideRestylers restylers overrides of
    Left errs -> undefined
    Right restylers -> pure $ filter rEnabled restylers

data RestylerOverride = RestylerOverride
  { name :: String
  , enabled :: Maybe Bool
  , image :: Maybe Image
  , command :: Maybe [String]
  , arguments :: Maybe [String]
  , include :: Maybe [Include]
  , interpreters :: Maybe [Interpreter]
  , delimiters :: Maybe Delimiters
  }
  deriving stock (Eq, Show, Generic)

-- instance FromJSON RestylerOverride where
--   parseJSON = \case
--     String name'
--       | Just name <- T.stripPrefix "!" name' ->
--           namedOverride (Key.fromText name)
--             $ KeyMap.singleton "enabled" (Bool False)
--     String name -> namedOverride (Key.fromText name) KeyMap.empty
--     Object o
--       | [(name, Object o')] <- KeyMap.toList o ->
--           namedOverride name o'
--     v ->
--       suffixIncorrectIndentation
--         $ genericParseJSON (aesonPrefix snakeCase) v

restylerOverridesParser :: Parser [RestylerOverride]
restylerOverridesParser = undefined

-- namedOverride :: Key -> KeyMap Value -> Parser RestylerOverride
-- namedOverride name =
--   parseJSON
--     . Object
--     . insertIfMissing "name" (String $ Key.toText name)
--     . KeyMap.delete name

-- suffixIncorrectIndentation :: Parser a -> Parser a
-- suffixIncorrectIndentation = modifyFailure (<> msg)
--  where
--   msg :: String
--   msg = "\n\nDo you have incorrect indentation for a named override?"

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
    let name = pack o.name
    in  case HashMap.lookup name restylers of
          Nothing -> Failure ["Unexpected Restyler name " <> show name]
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
