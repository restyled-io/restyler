{-# LANGUAGE QuasiQuotes #-}

module Restyler.Content
  ( pullRequestDescription
  ) where

import Restyler.Prelude

import GitHub.Data (unIssueNumber)
import Restyler.PullRequest
import Restyler.Restyler
import Restyler.RestylerResult
import Restyler.Wiki qualified as Wiki
import Text.Shakespeare.Text (st)

-- brittany-disable-next-binding

pullRequestDescription
  :: Maybe URL
  -- ^ Job URL, if we have it
  -> PullRequest
  -- ^ Original PR
  -> [RestylerResult]
  -> Text
pullRequestDescription mJobUrl pullRequest results
  | pullRequestIsFork pullRequest =
      [st|
A duplicate of ##{n} with additional commits that automatically address
incorrect style, created by [Restyled][].

:warning: Even though this PR is not a Fork, it contains outside contributions.
Please review accordingly.

Since the original Pull Request was opened as a fork in a contributor's
repository, we are unable to create a Pull Request branching from it with only
the style fixes.

The following Restylers #{madeFixes}:

#{resultsList}

To incorporate these changes, you can either:

1. Merge this Pull Request *instead of* the original, or

1. Ask your contributor to locally incorporate these commits and push them to
   the original Pull Request

   <details>
       <summary>Expand for example instructions</summary>

       ```console
       git remote add upstream #{getUrl $ pullRequestCloneUrl pullRequest}
       git fetch upstream pull/<this PR number>/head
       git merge --ff-only FETCH_HEAD
       git push
       ```

   </details>

#{footer}
|]
  | otherwise =
      [st|
Automated style fixes for ##{n}, created by [Restyled][].

The following restylers #{madeFixes}:

#{resultsList}

To incorporate these changes, merge this Pull Request into the original. We
recommend using the Squash or Rebase strategies.

#{footer}
|]
 where
  -- This variable is just so that we can wrap our content above such that
  -- when the link is rendered at ~3 digits, it looks OK.
  n = unIssueNumber $ pullRequestNumber pullRequest

  -- Link the "made fixes" line to the Job log, if we can
  madeFixes = case mJobUrl of
    Nothing -> "made fixes"
    Just jobUrl -> "[made fixes](" <> getUrl jobUrl <> ")"

  -- N.B. Assumes something committed changes, otherwise we'd not be opening
  -- this PR at all
  resultsList =
    unlines
      $ map (("- " <>) . restylerListItem . rrRestyler)
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
