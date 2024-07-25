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
  :: Int
  -- ^ Original PR Number
  -> NonEmpty RestylerResult
  -> Text
pullRequestDescription n results =
  [st|
Automated style fixes for ##{n}, created by [Restyled][].

The following restylers made fixes:

#{resultsList}

To incorporate these changes, merge this Pull Request into the original. We
recommend using the Squash or Rebase strategies.

**NOTE**: As work continues on the original Pull Request, this process will
re-run and update (force-push) this Pull Request with updated style fixes as
necessary. If the style is fixed manually at any point (i.e. this process finds
no fixes to make), this Pull Request will be closed automatically.

Sorry if this was unexpected. To disable it, see our [documentation][].

[restyled]: https://restyled.io
[documentation]: #{Wiki.page "Disabling Restyled"}
|]
 where
  resultsList =
    unlines
      $ map (("- " <>) . restylerListItem . (.restyler))
      $ filter restylerCommittedChanges
      $ toList results

  restylerListItem r = pack $ case rDocumentation r of
    (url : _) -> "[" <> rName r <> "](" <> url <> ")"
    _ -> rName r
