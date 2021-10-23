module Restyler.Main
    ( restylerMain
    ) where

import Restyler.Prelude

import qualified Data.Vector as V
import GitHub.Data.GitData (File(..))
import GitHub.Endpoints.PullRequests (FetchCount(..), pullRequestFilesR)
import Restyler.App.Class
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
       , HasGitHub env
       , HasDownloadFile env
       )
    => RIO env [RestylerResult]
restyle = do
    config <- view configL
    pullRequest <- view pullRequestL
    pullRequestPaths <- getChangedPaths pullRequest
    runRestylers config pullRequestPaths

wasRestyled :: (HasPullRequest env, HasGit env) => RIO env Bool
wasRestyled = do
    sha <- pullRequestHeadSha <$> view pullRequestL
    not . null <$> gitDiffNameOnly (Just $ unpack sha)

getChangedPaths :: HasGitHub env => PullRequest -> RIO env [FilePath]
getChangedPaths pullRequest = do
    files <- runGitHub $ pullRequestFilesR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestNumber pullRequest)
        FetchAll

    pure $ V.toList $ unpack . fileFilename <$> files
