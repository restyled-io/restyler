{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Restyler.Config.Interpreter
    ( Interpreter(..)
    , hasInterpreter
    ) where

import ClassyPrelude

import Data.Aeson
import System.FilePath (takeFileName)

import qualified Data.Text as T
import qualified Data.Text.IO as T

data Interpreter
    = Sh
    | Bash
    deriving (Eq, Show)

instance FromJSON Interpreter where
    parseJSON = withText "Interpreter" parseInterpreter

hasInterpreter :: FilePath -> Interpreter -> IO Bool
path `hasInterpreter` interpreter = do
    minterpreter <- getInterpreter path
    pure $ minterpreter == Just interpreter

getInterpreter :: FilePath -> IO (Maybe Interpreter)
getInterpreter path = do
    mshebang <- getShebang path `catch` errNothing
    pure $ interpreterForShebang =<< mshebang
  where
    errNothing :: IOException -> IO (Maybe a)
    errNothing _ = pure Nothing

getShebang :: FilePath -> IO (Maybe String)
getShebang path = do
    mline <- headMay . T.lines <$> T.readFile path
    pure $ T.unpack . T.strip <$> mline

interpreterForShebang :: String -> Maybe Interpreter
interpreterForShebang ('#':'!':path) = parseInterpreter $ takeFileName path
interpreterForShebang _ = Nothing

parseInterpreter :: (MonadPlus m, IsString a, Eq a) => a -> m Interpreter
parseInterpreter "sh" = pure Sh
parseInterpreter "bash" = pure Bash
parseInterpreter _ = mzero
