module Data.Yaml.Ext
  ( modifyInvalidYaml
  , modifyYamlProblem
  , locateErrorInContent
  ) where

import Prelude

import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Yaml

modifyInvalidYaml
  :: (YamlException -> YamlException) -> ParseException -> ParseException
modifyInvalidYaml f = \case
  ex@NonScalarKey {} -> ex
  ex@UnknownAlias {} -> ex
  ex@UnexpectedEvent {} -> ex
  InvalidYaml mEx -> InvalidYaml $ f <$> mEx
  ex@AesonException {} -> ex
  ex@OtherParseException {} -> ex
  ex@NonStringKey {} -> ex
  ex@NonStringKeyAlias {} -> ex
  ex@CyclicIncludes {} -> ex
  ex@LoadSettingsException {} -> ex
  ex@MultipleDocuments {} -> ex

modifyYamlProblem :: (String -> String) -> ParseException -> ParseException
modifyYamlProblem f = modifyInvalidYaml $ \case
  ex@YamlException {} -> ex
  YamlParseException problem context problemMark ->
    YamlParseException (f problem) context problemMark

locateErrorInContent :: ParseException -> Text -> Text
locateErrorInContent = \case
  AesonException {} -> id
  OtherParseException {} -> id
  NonStringKey {} -> id
  NonStringKeyAlias {} -> id
  CyclicIncludes {} -> id
  LoadSettingsException {} -> id
  MultipleDocuments {} -> id
  InvalidYaml (Just YamlParseException {..}) ->
    locateLineColumn
      (pack yamlProblem)
      (yamlLine yamlProblemMark)
      (yamlColumn yamlProblemMark)
  InvalidYaml {} -> id
  NonScalarKey {} -> id
  UnknownAlias {} -> id
  UnexpectedEvent {} -> id

locateLineColumn :: Text -> Int -> Int -> Text -> Text
locateLineColumn problem line column =
  T.unlines . uncurry insertMark . splitAt line . T.lines
 where
  insertMark before after = before <> marks <> after

  columnSpacing = T.replicate (column - 1) " "

  marks =
    [ columnSpacing <> "▲"
    , columnSpacing <> "│"
    , columnSpacing <> "└ " <> problem
    , ""
    ]
