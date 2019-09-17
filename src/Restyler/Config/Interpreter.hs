module Restyler.Config.Interpreter
    ( Interpreter(..)
    , hasInterpreter
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
    deriving (Eq, Show)

instance FromJSON Interpreter where
    parseJSON =
        withText "Interpreter" $ either fail pure . readInterpreter . unpack

instance ToJSON Interpreter where
    -- N.B. this may not always work, but it works for now
    toJSON = toJSON . T.toLower . tshow

-- | Does that path start with a /shebang/ for the given @'Interpreter'@
hasInterpreter :: Text -> Interpreter -> Bool
contents `hasInterpreter` interpreter = fromMaybe False $ do
    line <- headMaybe $ T.lines contents
    foundInterpreter <- parseInterpreter . unpack $ T.strip line
    pure $ foundInterpreter == interpreter

-- | TODO: Megaparsec?
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
