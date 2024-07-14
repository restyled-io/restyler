-- | @restyle path...@
--
-- - Run restylers with or without commits
--
-- TODO
module Restyler.Commands.RestyleLocal
  ( main
  , run
  ) where

import Restyler.Prelude

import Restyler.RestyleResult

main :: NonEmpty FilePath -> IO ()
main = void . run . toList

run :: Applicative m => [FilePath] -> m RestyleResult
run _ = pure RestyleSkippedNoPaths
