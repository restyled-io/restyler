-- |
--
-- Module      : Restyler.Docs
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Docs
  ( DocsPage (..)
  , renderDocsPage
  )
where

import Restyler.Prelude

import Mdoc.Gen
import OptEnvConf.Mdoc
import Restyler.Config (configParser, configPaths)

data DocsPage = Restyle1 | RestyledYaml5

renderDocsPage :: DocsPage -> IO ()
renderDocsPage =
  go . \case
    Restyle1 ->
      genMan1Template
        (ManTemplateFile "doc/restyle.1.template")
        "restyle"
        "restyle local files"
    RestyledYaml5 ->
      genMan5
        "restyled.yaml"
        "restyled configuration file"
        $ intercalate ", " configPaths
 where
  go :: (ManData -> IO Mdoc) -> IO ()
  go f = do
    mdoc <- f $ getManData $ configParser []
    putDoc ColorAuto stdout $ prettyMdoc mdoc
    exitSuccess
