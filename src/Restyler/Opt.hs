module Restyler.Opt
  ( parse
  , module Options.Applicative
  )
where

import Restyler.Prelude

import Data.List qualified as List
import Options.Applicative
import Options.Applicative.Help.Chunk

parse
  :: String
  -- ^ Description
  -> String
  -- ^ Footer
  -> Parser a
  -- ^ Options parser
  -> IO a
parse d f p =
  execParser
    $ info (p <**> helper)
    $ fullDesc
    <> progDesc d
    <> footerLines f

-- |
--
-- The 'footer' function takes a string, but then makes it a doc with
-- 'paragraph' which replaces all newlines with spaces. Fun. This function
-- preserves newlines at least, so the simple case of a pre-pretty-printed
-- string is hanlded somewhat better.
footerLines :: String -> InfoMod a
footerLines =
  footerDoc
    . Just
    . extractChunk
    . vcatChunks
    . map stringChunk
    . List.lines
