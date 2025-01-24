-- |
--
-- Module      : Restyler.Config.Interpreter
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Interpreter
  ( Interpreter (..)
  , readInterpreter
  ) where

import Restyler.Prelude

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Restyler.ReadP
import System.FilePath (takeFileName)

data Interpreter
  = Sh
  | Bash
  | Python
  | Ruby
  | Other Text
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Interpreter)

instance HasCodec Interpreter where
  codec =
    bimapCodec (Right . interpreterFromText) (T.toLower . pack . show) textCodec
      <?> "sh|bash|python|ruby|..."

readInterpreter :: Text -> Maybe Interpreter
readInterpreter contents = do
  line <- head <$> nonEmpty (lines contents)
  parseInterpreter . unpack $ T.strip line

parseInterpreter :: String -> Maybe Interpreter
parseInterpreter =
  either (const Nothing) (Just . interpreterFromText . pack) . parseReadP go
 where
  go :: ReadP String
  go = do
    void $ string "#!"
    benv <|> exec

  benv = string "/usr/bin/env " *> word
  exec = takeFileName <$> word

interpreterFromText :: Text -> Interpreter
interpreterFromText = \case
  "sh" -> Sh
  "bash" -> Bash
  "python" -> Python
  "python2" -> Python
  "python2.7" -> Python
  "python3" -> Python
  "python3.6" -> Python
  "ruby" -> Ruby
  x -> Other x
