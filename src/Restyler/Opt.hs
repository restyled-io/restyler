module Restyler.Opt
  ( parse
  , module Options.Applicative
  )
where

import Restyler.Prelude

import Options.Applicative

parse
  :: String
  -- ^ Description
  -> Parser a
  -- ^ Options parser
  -> IO a
parse d p = execParser $ info (p <**> helper) $ fullDesc <> progDesc d
