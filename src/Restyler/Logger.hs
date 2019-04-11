module Restyler.Logger
    ( restylerLogFunc
    ) where

import Restyler.Prelude hiding (takeWhile)

import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Restyler.Options
import System.Console.ANSI

restylerLogFunc :: Options -> LogFunc
restylerLogFunc Options {..} = mkLogFunc $ \_cs _source level msg ->
    when (level >= oLogLevel) $ do
        BS.putStr "["
        when oLogColor $ setSGR [levelStyle level]
        BS.putStr $ levelStr level
        when oLogColor $ setSGR [Reset]
        BS.putStr "] "
        BS.putStrLn $ toStrictBytes $ toLazyByteString $ getUtf8Builder msg

levelStr :: LogLevel -> ByteString
levelStr = \case
    LevelDebug -> "DEBUG"
    LevelInfo -> "INFO"
    LevelWarn -> "WARN"
    LevelError -> "ERROR"
    LevelOther x -> encodeUtf8 $ T.toUpper x

levelStyle :: LogLevel -> SGR
levelStyle = \case
    LevelDebug -> SetColor Foreground Dull Magenta
    LevelInfo -> SetColor Foreground Dull Blue
    LevelWarn -> SetColor Foreground Dull Yellow
    LevelError -> SetColor Foreground Dull Red
    LevelOther _ -> Reset
