-- |
--
-- Module      : Restyler.Wiki
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Wiki
  ( commonError
  , page
  ) where

import Restyler.Prelude

import Data.Text qualified as T

commonError :: Text -> Text
commonError = page . ("Common Errors: " <>)

page :: Text -> Text
page = (wikiPrefix <>) . autoSlashPrefix . autoHyphenate

autoHyphenate :: Text -> Text
autoHyphenate = T.replace " " "-"

autoSlashPrefix :: Text -> Text
autoSlashPrefix x
  | "/" `T.isPrefixOf` x = x
  | otherwise = "/" <> x

wikiPrefix :: Text
wikiPrefix = "https://github.com/restyled-io/restyled.io/wiki"
