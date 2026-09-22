{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

import Mdoc
import Mdoc.Template qualified as Template
import OptEnvConf.Mdoc
import Restyler.Config (configParser)

data DocsPage = Restyle1 | RestyledYaml5

renderDocsPage :: DocsPage -> IO ()
renderDocsPage dp = do
  let page = getPage (configParser [])

  mdoc <- case dp of
    Restyle1 -> do
      t <- Template.custom "doc/restyle.1.mustache"
      page
        & name "restyle" "restyle local files"
        & renderTemplateThrow t
    RestyledYaml5 -> do
      -- TODO: add configPaths
      t <- Template.custom "doc/restyled.yaml.5.mustache"
      page
        & name "restyled.yaml" "restyled configuration file"
        & renderTemplateThrow t

  putMdoc mdoc
  exitSuccess
