module Restyler.Wiki
    ( commonError
    , page
    ) where

import Restyler.Prelude

import qualified Data.Text as T

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
