{-# LANGUAGE LambdaCase #-}

module Restyler.Logger
    ( restylerLogFunc
    , restylerLogFunc'
    )
where

import Restyler.Prelude

import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import Restyler.Options
import System.Console.ANSI

restylerLogFunc :: Options -> LogFunc
restylerLogFunc Options {..} = restylerLogFunc' oLogLevel oLogColor

-- | @'restylerLogFunc'@, but not dependent on a full @'Options'@ value
restylerLogFunc' :: LogLevel -> Bool -> LogFunc
restylerLogFunc' logLevel logColor = mkLogFunc $ \_cs _source level msg ->
    when (level >= logLevel) $ do
        BS8.putStr "["
        when logColor $ setSGR [levelStyle level]
        BS8.putStr $ levelStr level
        when logColor $ setSGR [Reset]
        BS8.putStr "] "
        BS8.putStrLn $ toStrictBytes $ toLazyByteString $ getUtf8Builder msg

levelStr :: LogLevel -> ByteString
levelStr = \case
    LevelDebug -> "Debug"
    LevelInfo -> "Info"
    LevelWarn -> "Warn"
    LevelError -> "Error"
    LevelOther x -> encodeUtf8 x

levelStyle :: LogLevel -> SGR
levelStyle = \case
    LevelDebug -> SetColor Foreground Dull Magenta
    LevelInfo -> SetColor Foreground Dull Blue
    LevelWarn -> SetColor Foreground Dull Yellow
    LevelError -> SetColor Foreground Dull Red
    LevelOther _ -> Reset
