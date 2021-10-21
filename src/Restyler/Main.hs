module Restyler.Main
    ( restylerMain
    ) where

import Restyler.Prelude

import Restyler.App.Class
import Restyler.Comment
import Restyler.Config
import Restyler.Git
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequest.Status
import Restyler.RestyledPullRequest
import Restyler.Restyler.Run
import Restyler.RestylerResult

restylerMain
    :: ( HasLogFunc env
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       , HasRestyledPullRequest env
       , HasSystem env
       , HasExit env
       , HasProcess env
       , HasGit env
       , HasDownloadFile env
       , HasGitHub env
       )
    => RIO env a
restylerMain = do
    results <- restyle
    logDebug $ "Restyling results: " <> displayIntercalated ", " results

    jobUrl <- oJobUrl <$> view optionsL
    pullRequest <- view pullRequestL
    mRestyledPullRequest <- view restyledPullRequestL

    unlessM wasRestyled $ do
        clearRestyledComments pullRequest
        traverse_ closeRestyledPullRequest mRestyledPullRequest
        sendPullRequestStatus $ NoDifferencesStatus jobUrl
        exitWithInfo "No style differences found"

    -- NB there is the edge-case of switching this off mid-PR. A previously
    -- opened Restyle PR would stop updating at that point.
    whenConfig (not . cPullRequests) $ do
        sendPullRequestStatus $ DifferencesStatus jobUrl
        exitWithInfo "Not creating (or updating) Restyle PR, disabled by config"

    url <- restyledPullRequestHtmlUrl <$> case mRestyledPullRequest of
        Nothing -> createRestyledPullRequest pullRequest results
        Just pr -> updateRestyledPullRequest pr results

    sendPullRequestStatus $ DifferencesStatus $ Just url
    exitWithInfo "Restyling successful"

restyle
    :: ( HasLogFunc env
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       , HasSystem env
       , HasProcess env
       , HasGit env
       , HasDownloadFile env
       )
    => RIO env [RestylerResult]
restyle = do
    config <- view configL
    pullRequest <- view pullRequestL
    pullRequestPaths <- changedPaths $ pullRequestBaseSha pullRequest
    runRestylers config pullRequestPaths

wasRestyled :: (HasPullRequest env, HasGit env) => RIO env Bool
wasRestyled = do
    headSha <- pullRequestHeadSha <$> view pullRequestL
    not . null <$> changedPaths headSha

changedPaths :: HasGit env => Text -> RIO env [FilePath]
changedPaths = gitDiffNameOnly . Just . unpack
