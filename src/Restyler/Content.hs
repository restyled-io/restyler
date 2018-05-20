{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Restyler.Content
    ( commitMessage
    , commentBody
    ) where

import Data.Text (Text)
import GitHub.Data (PullRequest(..))
import Restyler.PullRequest
import Text.Shakespeare.Text (st)

commitMessage :: Text
commitMessage = "Restyled"

commentBody :: PullRequest -> Text
commentBody pullRequest = [st|
Hi there!

I just wanted to let you know that some code in this PR might not match the
team's preferred styles. This process isn't perfect, but when we ran some
auto-reformatting tools on it there were differences. Those differences can be
seen in ##{pullRequestNumber pullRequest}.

#{commentCTA pullRequest}

Thanks,
[Restyled.io][]

[restyled.io]: https://restyled.io
[documentation]: https://github.com/restyled-io/restyled.io/wiki/Disabling-Restyled
|]

commentCTA :: PullRequest -> Text
commentCTA pullRequest
    | pullRequestIsFork pullRequest = [st|
Your PR was opened from a fork, so we're unable to open our PR with yours as the
base branch. Therefore, the PR linked above was opened directly against
`#{bBranch}`. It includes your changes and another commit to adjust styling.

If you're interested in incorporating the style changes in your PR, you can do
that locally with something like:

```console
git remote add upstream #{pullRequestRepoURL pullRequest}
git fetch upstream pull/#{pullRequestNumber pullRequest}/head
git merge --ff-only FETCH_HEAD
git push
```
|]
    | otherwise = [st|
To incorporate the changes, merge that PR into yours.

Sorry if this was unexpected. To disable it, see our [documentation].
|]
  where
    bBranch = pullRequestBaseRef pullRequest
