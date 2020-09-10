module Restyler.Config.Interpreter
    ( Interpreter(..)
    , readInterpreter
    )
where

import Restyler.Prelude

import Data.Aeson
import qualified Data.Text as T
import System.FilePath (takeFileName)

data Interpreter
    = Sh
    | Bash
    | Python
    | Ruby
    deriving stock (Eq, Show)

instance FromJSON Interpreter where
    parseJSON =
        withText "Interpreter"
            $ either fail pure
            . intepreterFromString
            . unpack

instance ToJSON Interpreter where
    -- N.B. this may not always work, but it works for now
    toJSON = toJSON . T.toLower . tshow

readInterpreter :: Text -> Maybe Interpreter
readInterpreter contents = do
    line <- headMaybe $ T.lines contents
    parseInterpreter . unpack $ T.strip line

-- | TODO: Megaparsec?
parseInterpreter :: String -> Maybe Interpreter
parseInterpreter ('#' : '!' : rest) =
    hush $ intepreterFromString =<< case words rest of
        [exec] -> pure $ takeFileName exec
        ["/usr/bin/env", arg] -> pure arg
        _ -> Left "Unexpected shebang length"

parseInterpreter _ = Nothing

intepreterFromString :: String -> Either String Interpreter
intepreterFromString "sh" = Right Sh
intepreterFromString "bash" = Right Bash
intepreterFromString "python" = Right Python
intepreterFromString "python2" = Right Python
intepreterFromString "python2.7" = Right Python
intepreterFromString "python3" = Right Python
intepreterFromString "python3.6" = Right Python
intepreterFromString "ruby" = Right Ruby
intepreterFromString x = Left $ "Unknown executable: " <> x
