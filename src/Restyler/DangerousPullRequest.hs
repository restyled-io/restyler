module Restyler.DangerousPullRequest
  ( checkDangerousPullRequest
  ) where

import Restyler.Prelude

import Restyler.GitHub.PullRequest

checkDangerousPullRequest :: PullRequest -> Maybe Text
checkDangerousPullRequest pr
  | pr.base.repo.private = Nothing
  | pr.base.repo.owner.login == pr.head.repo.owner.login = Nothing
  | otherwise =
      Just "Forks in open source projects could contain unsafe contributions"
