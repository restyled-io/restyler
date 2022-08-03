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
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadReader env m
       , MonadSystem m
       , MonadExit m
       , MonadProcess m
       , MonadGit m
       , MonadGitHub m
       , MonadDownloadFile m
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       , HasRestyledPullRequest env
       )
    => m a
restylerMain = do
    results <- restyle
    -- logDebug $ "Restyled" :# ["results" .= results]

    jobUrl <- oJobUrl <$> view optionsL
    pullRequest <- view pullRequestL

    mRestyledPullRequest <- view restyledPullRequestL

    unlessM wasRestyled $ do
        traverse_ closeRestyledPullRequest mRestyledPullRequest
        sendPullRequestStatus $ NoDifferencesStatus jobUrl
        exitWithInfo "No style differences found"

    whenConfig cAuto $ do
        if pullRequestIsFork pullRequest
            then logWarn "Ignoring auto:true because PR is a fork"
            else do
                logInfo "Pushing changes directly to PR branch"
                gitPush
                    $ unpack
                    $ pullRequestLocalHeadRef pullRequest
                    <> ":"
                    <> pullRequestHeadRef pullRequest
                exitWithInfo "Restyling successful"

    -- NB there is the edge-case of switching this off mid-PR. A previously
    -- opened Restyle PR would stop updating at that point.
    whenConfig (not . cPullRequests) $ do
        sendPullRequestStatus $ DifferencesStatus jobUrl
        exitWithInfo "Not creating (or updating) Restyle PR, disabled by config"

    let isDangerous =
            pullRequestRepoPublic pullRequest && pullRequestIsFork pullRequest

        wiki :: Text
        wiki
            = "https://github.com/restyled-io/restyled.io/wiki/2022.08.03-Attack-on-Forked-PRs"

    when isDangerous $ do
        sendPullRequestStatus $ DifferencesStatus jobUrl
        logInfo "Not creating Restyle PR, it could contain unsafe contributions"
        exitWithInfo $ "Please see " <> wiki :# []

    url <- restyledPullRequestHtmlUrl <$> case mRestyledPullRequest of
        Nothing -> createRestyledPullRequest pullRequest results
        Just pr -> updateRestyledPullRequest pullRequest pr results

    sendPullRequestStatus $ DifferencesStatus $ Just url
    exitWithInfo "Restyling successful"

restyle
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadSystem m
       , MonadProcess m
       , MonadGit m
       , MonadGitHub m
       , MonadDownloadFile m
       , MonadReader env m
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       )
    => m [RestylerResult]
restyle = do
    config <- view configL
    pullRequest <- view pullRequestL
    pullRequestPaths <- getChangedPaths pullRequest
    runRestylers config pullRequestPaths

wasRestyled :: (MonadGit m, MonadReader env m, HasPullRequest env) => m Bool
wasRestyled = do
    sha <- pullRequestHeadSha <$> view pullRequestL
    not . null <$> gitDiffNameOnly (Just $ unpack sha)

getChangedPaths :: MonadGitHub m => PullRequest -> m [FilePath]
getChangedPaths pullRequest = do
    files <- runGitHub $ pullRequestFilesR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestNumber pullRequest)
        FetchAll

    pure $ V.toList $ unpack . fileFilename <$> files
