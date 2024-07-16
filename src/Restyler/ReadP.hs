module Restyler.ReadP
  ( parseReadP
  , module Text.ParserCombinators.ReadP
  , textTill1
  , charsTill1
  , word
  , digits
  ) where

import Restyler.Prelude

import Data.Char (isDigit)
import Data.List.NonEmpty as NE
import Text.ParserCombinators.ReadP hiding (option)
import Prelude qualified as Unsafe

parseReadP :: ReadP a -> String -> Either String a
parseReadP p s = case NE.nonEmpty (readP_to_S p s) of
  Nothing -> Left "No parse"
  Just ne -> case NE.last ne of
    (a, []) -> Right a
    (_, xs) -> Left $ "Input remaining: " <> show xs

textTill1 :: Char -> ReadP Text
textTill1 = fmap pack . charsTill1

charsTill1 :: Char -> ReadP String
charsTill1 c = do
  as <- many1 (satisfy (/= c))
  as <$ char c

digits :: ReadP Int
digits = Unsafe.read <$> many1 (satisfy isDigit)

word :: ReadP String
word = many1 $ satisfy $ not . isSpace
