module Restyler.Config.Interpreter (
    Interpreter (..),
    readInterpreter,
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
    | Other Text
    deriving stock (Eq, Show)

instance FromJSON Interpreter where
    parseJSON = withText "Interpreter" $ pure . intepreterFromText

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
    intepreterFromText <$> case words (pack rest) of
        [exec] -> pure $ pack $ takeFileName $ unpack exec
        ["/usr/bin/env", arg] -> pure arg
        _ -> Nothing
parseInterpreter _ = Nothing

intepreterFromText :: Text -> Interpreter
intepreterFromText "sh" = Sh
intepreterFromText "bash" = Bash
intepreterFromText "python" = Python
intepreterFromText "python2" = Python
intepreterFromText "python2.7" = Python
intepreterFromText "python3" = Python
intepreterFromText "python3.6" = Python
intepreterFromText "ruby" = Ruby
intepreterFromText x = Other x
