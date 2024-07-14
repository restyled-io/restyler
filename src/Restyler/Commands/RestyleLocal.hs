-- | @restyle path...@
--
-- - Run restylers with or without commits
module Restyler.Commands.RestyleLocal
  ( run
  ) where

import Restyler.Prelude

import Restyler.RestylerResult

run :: NonEmpty FilePath -> m [RestylerResult]
run _ = error "TODO"
