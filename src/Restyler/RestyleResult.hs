module Restyler.RestyleResult
  ( RestyleResult (..)
  , logRestyleResult
  , logRestylerResult
  ) where

import Restyler.Prelude

import Restyler.Ignore
import Restyler.Restyler
import Restyler.RestylerResult

data RestyleResult
  = RestyleSkippedClosed
  | RestyleSkippedDraft
  | RestyleSkippedIgnored IgnoredReason
  | RestyleSkippedNoRestylers
  | RestyleSkippedNoPaths
  | Restyled (NonEmpty RestylerResult)

logRestyleResult :: MonadLogger m => RestyleResult -> m ()
logRestyleResult = \case
  RestyleSkippedClosed -> logInfo $ "Restyle skipped" :# ["reason" .= t "closed"]
  RestyleSkippedDraft -> logInfo $ "Restyle skipped" :# ["reason" .= t "closed"]
  RestyleSkippedIgnored reason -> logInfo $ "Restyle skipped" :# ["reason" .= show @Text reason]
  RestyleSkippedNoRestylers -> logInfo $ "Restyle skipped" :# ["reason" .= t "No Restylers"]
  RestyleSkippedNoPaths -> logInfo $ "Restyle skipped" :# ["reason" .= t "No paths"]
  Restyled results -> traverse_ logRestylerResult results

logRestylerResult :: MonadLogger m => RestylerResult -> m ()
logRestylerResult RestylerResult {..} =
  case rrOutcome of
    ChangesCommitted changed sha -> do
      logInfo
        $ "Changes committed"
        :# [ "restyler" .= rName rrRestyler
           , "paths" .= length changed
           , "sha" .= sha
           ]
    x -> logDebug $ "" :# ["result" .= x]

t :: Text -> Text
t = id
