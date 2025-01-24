-- |
--
-- Module      : Restyler.ReadP
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.ReadP
  ( parseReadP
  , module Text.ParserCombinators.ReadP
  , word
  ) where

import Restyler.Prelude

import Text.ParserCombinators.ReadP hiding (option)

parseReadP :: ReadP a -> String -> Either String a
parseReadP p s = case nonEmpty (readP_to_S p s) of
  Nothing -> Left "No parse"
  Just ne -> case last ne of
    (a, []) -> Right a
    (_, xs) -> Left $ "Input remaining: " <> show xs

word :: ReadP String
word = many1 $ satisfy $ not . isSpace
