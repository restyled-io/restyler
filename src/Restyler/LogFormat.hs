{-# LANGUAGE RecordWildCards #-}

-- |
--
-- Module      : Restyler.LogFormat
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.LogFormat
  ( getSetReformat
  ) where

import Restyler.Prelude

import Blammo.Logging.Colors
import Blammo.Logging.LogSettings (LogSettings)
import Blammo.Logging.Logger (setLoggerReformat)
import Control.Monad.Logger.Aeson
import Data.Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T

getSetReformat :: Bool -> (Logger -> Logger)
getSetReformat isGHA = do
  setLoggerReformat
    $ if isGHA
      then reformatLoggedMessage gray
      else reformatLoggedMessage dim

reformatLoggedMessage
  :: (Colors -> Text -> Text)
  -> LogSettings
  -> Colors
  -> LogLevel
  -> LoggedMessage
  -> ByteString
reformatLoggedMessage getDim _ colors@Colors {..} logLevel LoggedMessage {..} =
  encodeUtf8
    $ mconcat
      [ case logLevel of
          LevelDebug -> blue $ fixedWidth levelWidth "DEBUG" <> ":"
          LevelInfo -> green $ fixedWidth levelWidth "INFO" <> ":"
          LevelWarn -> yellow $ fixedWidth levelWidth "WARN" <> ":"
          LevelError -> red $ fixedWidth levelWidth "ERROR" <> ":"
          LevelOther x -> gray $ fixedWidth levelWidth x <> ":"
      , " " <> loggedMessageText
      , fromMaybe "" $ do
          guard $ not $ KeyMap.null metaKeyMap
          pure $ encodePretty (getDim colors) $ Object metaKeyMap
      ]
 where
  levelWidth :: Int
  levelWidth = 5

  metaKeyMap =
    maybe mempty (KeyMap.singleton "source" . String) loggedMessageLogSource
      <> loggedMessageThreadContext
      <> loggedMessageMeta

encodePretty :: (Text -> Text) -> Value -> Text
encodePretty dim =
  mconcat
    . map (\x -> "\n" <> indent <> dim x)
    . T.lines
    . decodeUtf8
    . BSL.toStrict
    . Pretty.encodePretty' conf
 where
  indent = T.replicate 7 " "
  conf = Pretty.defConfig {Pretty.confIndent = Pretty.Spaces 2}

fixedWidth :: Int -> Text -> Text
fixedWidth n t = case compare len n of
  EQ -> t
  GT -> T.take n t
  LT -> T.replicate (n - len) " " <> t
 where
  len = T.length t
