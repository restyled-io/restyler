{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Restyler.Content
    ( commitMessage
    , commentBody
    , commentBodyFork
    , pullRequestBody
    ) where

import Restyler.Prelude hiding (commentBody, pullRequestBody)

import Restyler.Model.PullRequest
import Restyler.Model.Restyler
import Text.Shakespeare.Text (st)

-- | Simple for now: @Restyled@
commitMessage :: Text
commitMessage = "Restyled"

commentBody :: PullRequest -> Text
commentBody pullRequest = mconcat
    [ commentPreamble pullRequest <> "\n"
    , commentToIncorporate <> "\n"
    , commentFooter
    ]

commentBodyFork :: PullRequest -> Text
commentBodyFork pullRequest = mconcat
    [ commentPreamble pullRequest <> "\n"
    , commentToIncorporateFork pullRequest <> "\n"
    , commentFooter
    ]

-- brittany-disable-next-binding

commentPreamble :: PullRequest -> Text
commentPreamble pullRequest = [st|
Hey there-

I'm a [bot][homepage], here to let you know that some code in this PR might not
match the team's automated styling. I ran the team's auto-reformatting tools on
the files changed in this PR and found some differences. Those differences can
be seen in ##{pullRequestNumber pullRequest}.
|]

-- brittany-disable-next-binding

commentToIncorporate :: Text
commentToIncorporate = [st|
To incorporate those fixes, just merge that PR into this one. Or, if you
manually fix the styling, that PR will be closed and this comment deleted.
|]

-- brittany-disable-next-binding

commentToIncorporateFork :: PullRequest -> Text
commentToIncorporateFork pullRequest = [st|
**NOTE**: Since this PR was opened from a fork, we're not able to open our PR
with yours as the base branch. Therefore, the PR linked above was opened
directly against `#{pullRequestBaseRef pullRequest}`. It includes your changes and another commit to
adjusted styling.

If you're interested in incorporating the style fixes in this PR, you can do
that locally with something like:

```console
git remote add upstream #{getUrl $ pullRequestCloneUrl pullRequest}
git fetch upstream pull/#{pullRequestNumber pullRequest}/head
git merge --ff-only FETCH_HEAD
git push
```

Fixing the styling (through the above or any other means) will cause the linked
PR to be closed and this comment delete.
|]

-- brittany-disable-next-binding

commentFooter :: Text
commentFooter = [st|
Sorry if this was unexpected. To disable it, see our [documentation][].

[homepage]: https://restyled.io
[documentation]: https://github.com/restyled-io/restyled.io/wiki/Disabling-Restyled
|]

pullRequestBody :: PullRequest -> [Restyler] -> Text
pullRequestBody _pullRequest _restylers = ""
