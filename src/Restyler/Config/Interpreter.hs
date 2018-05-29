{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Restyler.Config.Interpreter
    ( Interpreter(..)
    , hasInterpreter
    )
where

import Restyler.Prelude.NoApp

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath (takeFileName)

data Interpreter
    = Sh
    | Bash
    | Python
    | Ruby
    deriving (Eq, Show)

instance FromJSON Interpreter where
    parseJSON = withText "Interpreter"
        $ either fail pure . readInterpreter . unpack

hasInterpreter :: FilePath -> Interpreter -> IO Bool
path `hasInterpreter` interpreter = do
    minterpreter <- getInterpreter path
    pure $ minterpreter == Just interpreter

getInterpreter :: FilePath -> IO (Maybe Interpreter)
getInterpreter path = handleIO (const $ pure Nothing) $ do
    mline <- headMay . T.lines <$> T.readFile path
    pure $ parseInterpreter . unpack . T.strip =<< mline

parseInterpreter :: String -> Maybe Interpreter
parseInterpreter ('#' : '!' : rest) =
    hush $ readInterpreter =<< case words rest of
        [exec] -> pure $ takeFileName exec
        ["/usr/bin/env", arg] -> pure arg
        _ -> Left "Unexpected shebang length"

parseInterpreter _ = Nothing

readInterpreter :: String -> Either String Interpreter
readInterpreter "sh" = Right Sh
readInterpreter "bash" = Right Bash
readInterpreter "python" = Right Python
readInterpreter "python2" = Right Python
readInterpreter "python2.7" = Right Python
readInterpreter "python3" = Right Python
readInterpreter "python3.6" = Right Python
readInterpreter "ruby" = Right Ruby
readInterpreter x = Left $ "Unknown executable: " <> x
