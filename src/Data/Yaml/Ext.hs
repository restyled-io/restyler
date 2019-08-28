{-# LANGUAGE LambdaCase #-}

module Data.Yaml.Ext
    ( modifyInvalidYaml
    , modifyYamlProblem
    )
where

import Prelude

import Data.Yaml

modifyInvalidYaml
    :: (YamlException -> YamlException) -> ParseException -> ParseException
modifyInvalidYaml f = \case
    ex@NonScalarKey{} -> ex
    ex@UnknownAlias{} -> ex
    ex@UnexpectedEvent{} -> ex

    InvalidYaml mEx -> InvalidYaml $ f <$> mEx

    ex@AesonException{} -> ex
    ex@OtherParseException{} -> ex
    ex@NonStringKeyAlias{} -> ex
    ex@CyclicIncludes{} -> ex

modifyYamlProblem :: (String -> String) -> ParseException -> ParseException
modifyYamlProblem f = modifyInvalidYaml $ \case
    ex@YamlException{} -> ex
    YamlParseException problem context problemMark ->
        YamlParseException (f problem) context problemMark
