module Restyler.Config.Interpreter
    ( Interpreter(..)
    , readInterpreter
    ) where

import Restyler.Prelude

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import System.FilePath (takeFileName)

data Interpreter
    = Sh
    | Bash
    | Python
    | Ruby
    deriving stock (Eq, Show)

instance FromJSON Interpreter where
    parseJSON = withText "Interpreter" $ either fail pure . intepreterFromText

instance ToJSON Interpreter where
    -- N.B. this may not always work, but it works for now
    toJSON = toJSON . T.toLower . show

readInterpreter :: Text -> Maybe Interpreter
readInterpreter contents = do
    line <- head <$> NE.nonEmpty (lines contents)
    parseInterpreter . unpack $ T.strip line

-- | TODO: Megaparsec?
parseInterpreter :: String -> Maybe Interpreter
parseInterpreter ('#' : '!' : rest) =
    hush $ intepreterFromText =<< case words (pack rest) of
        [exec] -> pure $ pack $ takeFileName $ unpack exec
        ["/usr/bin/env", arg] -> pure arg
        _ -> Left "Unexpected shebang length"

parseInterpreter _ = Nothing

intepreterFromText :: Text -> Either String Interpreter
intepreterFromText "sh" = Right Sh
intepreterFromText "bash" = Right Bash
intepreterFromText "python" = Right Python
intepreterFromText "python2" = Right Python
intepreterFromText "python2.7" = Right Python
intepreterFromText "python3" = Right Python
intepreterFromText "python3.6" = Right Python
intepreterFromText "ruby" = Right Ruby
intepreterFromText x = Left $ "Unknown executable: " <> show x
