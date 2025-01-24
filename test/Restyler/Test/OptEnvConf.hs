-- |
--
-- Module      : Restyler.Test.OptEnvConf
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Test.OptEnvConf
  ( Parser
  , ParseError (..)
  , ParseErrorMessage (..)
  , parseErrorsText

    -- * Running
  , runParser
  , runParserEither

    -- * Constructing defaults
  , Object
  , object
  , mkObject
  , (.=)

    -- * Expectations
  , shouldHaveParseError
  ) where

import Restyler.Prelude

import Data.Aeson (Object, object)
import Data.Aeson.Types (Pair)
import OptEnvConf (Parser)
import OptEnvConf.Args (parseArgs)
import OptEnvConf.EnvMap qualified as EnvMap
import OptEnvConf.Error (ParseError (..), ParseErrorMessage (..), renderErrors)
import OptEnvConf.Run (runParserOn)
import Test.Hspec
import Text.Colour (chunkText)

parseErrorsText :: NonEmpty ParseError -> Text
parseErrorsText = mconcat . map chunkText . renderErrors

-- | Run the 'Parser' on options, environment, and (maybe) config
--
-- Throw 'expectationFailure' if parsing fails. See 'runParserEither'.
runParser
  :: HasCallStack
  => Parser a
  -> [String]
  -- ^ Args
  -> [(String, String)]
  -- ^ ENV
  -> Maybe Object
  -- ^ Maybe configuration object
  -> IO a
runParser p opt env conf = do
  runParserEither p opt env conf >>= \case
    Left errs -> do
      expectationFailure $ unpack $ parseErrorsText errs
      error "unreachable"
    Right a -> pure a

-- | Run the 'Parser' on options, environment, and (maybe) config
runParserEither
  :: Parser a
  -> [String]
  -- ^ Args
  -> [(String, String)]
  -- ^ ENV
  -> Maybe Object
  -- ^ Maybe configuration object
  -> IO (Either (NonEmpty ParseError) a)
runParserEither p args = runParserOn Nothing p (parseArgs args) . EnvMap.parse

mkObject :: [Pair] -> Object
mkObject = fromList

shouldHaveParseError
  :: Either (NonEmpty ParseError) a -> (ParseErrorMessage -> Bool) -> IO ()
shouldHaveParseError result p = go $ first toList result
 where
  go = \case
    Left [] ->
      expectationFailure
        $ "ParseErrors did not match expectation: "
        <> show (void result)
    Left (e : es)
      | ParseError _ em <- e, p em -> pure ()
      | otherwise -> go $ Left es
    Right {} ->
      expectationFailure "Expected ParseErrors, but parsing succeeded"

infix 1 `shouldHaveParseError`
