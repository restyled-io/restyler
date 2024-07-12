{-# LANGUAGE NamedFieldPuns #-}

module Restyler.GitHub.Repository
  ( Repository (..)
  , parseRepository
  ) where

import Restyler.Prelude

import Data.Text qualified as T

data Repository = Repository
  { owner :: Text
  , repo :: Text
  }

parseRepository :: String -> Either String Repository
parseRepository = go . T.splitOn "/" . pack
 where
  go :: [Text] -> Either String Repository
  go = \case
    [owner, repo]
      | not (T.null owner)
      , not (T.null repo) ->
          Right $ Repository {owner, repo}
    _ -> Left "Invalid repo, must be OWNER/REPO"
