{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Logger
    ( runAppLoggingT
    )
where

import Restyler.Prelude hiding (takeWhile)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Restyler.App.Type
import Scanner
import System.Console.ANSI
import System.Log.FastLogger (fromLogStr)

runAppLoggingT :: MonadIO m => App -> LoggingT m a -> m a
runAppLoggingT App {..} = runLogging
    . filterLogger (\_ level -> level >= appLogLevel)
  where
    runLogging =
        if appLogColor then runStdoutANSILoggerT else runStdoutLoggingT

runStdoutANSILoggerT :: LoggingT m a -> m a
runStdoutANSILoggerT = (`runLoggingT` logger)
  where
    logger loc src level str = do
        let
            (mLevelStr, logStr) =
                splitLogStr . fromLogStr $ defaultLogStr loc src level str

        for_ mLevelStr $ \levelStr -> do
            BS.putStr "["
            setSGR [levelStyle level]
            BS.putStr levelStr
            setSGR [Reset]
            BS.putStr "] "

        BS.putStr logStr

-- | Split a log message into level and message
--
-- >>> :set -XOverloadedStrings
--
-- >>> splitLogStr "[Debug] Foo bar baz"
-- (Just "Debug","Foo bar baz")
--
-- >>> splitLogStr "[Info#123] Foo bar baz"
-- (Just "Info#123","Foo bar baz")
--
-- >>> splitLogStr "[Nope nope"
-- (Nothing,"[Nope nope")
--
splitLogStr :: ByteString -> (Maybe ByteString, ByteString)
splitLogStr bs = case scanOnly logScanner bs of
    Left _ -> (Nothing, bs)
    Right (x, y) -> (Just x, y)

logScanner :: Scanner (ByteString, ByteString)
logScanner =
    (,)
        <$> (char8 '[' *> takeWhileChar8 (/= ']') <* char8 ']')
        <*> (skipSpace *> takeWhile (const True))

levelStyle :: LogLevel -> SGR
levelStyle = \case
    LevelDebug -> SetColor Foreground Dull Magenta
    LevelInfo -> SetColor Foreground Dull Blue
    LevelWarn -> SetColor Foreground Dull Yellow
    LevelError -> SetColor Foreground Dull Red
    LevelOther _ -> Reset
