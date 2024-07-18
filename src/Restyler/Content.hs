{-# LANGUAGE QuasiQuotes #-}

module Restyler.Content
  ( pullRequestDescription
  ) where

import Restyler.Prelude

import Restyler.Restyler
import Restyler.RestylerResult
import Restyler.Wiki qualified as Wiki
import Text.Shakespeare.Text (st)

pullRequestDescription
  :: Maybe URL
  -- ^ Job URL, if we have it
  -> Int
  -- ^ Original PR Number
  -> [RestylerResult]
  -> Text
pullRequestDescription mJobUrl n results =
  [st|
Automated style fixes for ##{n}, created by [Restyled][].

The following restylers #{madeFixes}:

#{resultsList}

To incorporate these changes, merge this Pull Request into the original. We
recommend using the Squash or Rebase strategies.

#{footer}
|]
 where
  -- Link the "made fixes" line to the Job log, if we can
  madeFixes = case mJobUrl of
    Nothing -> "made fixes"
    Just jobUrl -> "[made fixes](" <> getUrl jobUrl <> ")"

  -- N.B. Assumes something committed changes, otherwise we'd not be opening
  -- this PR at all
  resultsList =
    unlines
      $ map (("- " <>) . restylerListItem . (.restyler))
      $ filter restylerCommittedChanges results

  restylerListItem Restyler {..} = pack $ case rDocumentation of
    (url : _) -> "[" <> rName <> "](" <> url <> ")"
    _ -> rName

  footer =
    [st|
**NOTE**: As work continues on the original Pull Request, this process will
re-run and update (force-push) this Pull Request with updated style fixes as
necessary. If the style is fixed manually at any point (i.e. this process finds
no fixes to make), this Pull Request will be closed automatically.

Sorry if this was unexpected. To disable it, see our [documentation][].

[restyled]: https://restyled.io
[documentation]: #{Wiki.page "Disabling Restyled"}
|]
