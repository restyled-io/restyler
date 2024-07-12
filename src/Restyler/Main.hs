module Restyler.Main
  ( restylerMain
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Data.Vector qualified as V
import GitHub.Data.GitData (File (..))
import GitHub.Endpoints.PullRequests (FetchCount (..), pullRequestFilesR)
import Restyler.App.Class
import Restyler.Config
import Restyler.Git
import Restyler.HostDirectoryOption
import Restyler.ImageCleanupOption
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequest.Status
import Restyler.Restrictions
import Restyler.RestyledPullRequest
import Restyler.Restyler.Run
import Restyler.RestylerResult

restylerMain
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadExit m
     , MonadProcess m
     , MonadGit m
     , MonadGitHub m
     , MonadDownloadFile m
     , MonadReader env m
     , HasLogger env
     , HasOptions env
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasRestrictions env
     , HasConfig env
     , HasPullRequest env
     )
  => m a
restylerMain = do
  results <- restyle

  mJobUrl <- oJobUrl <$> view optionsL
  pullRequest <- view pullRequestL

  unlessM wasRestyled $ do
    mRestyledPullRequest <- findRestyledPullRequest pullRequest
    traverse_ closeRestyledPullRequest mRestyledPullRequest
    sendPullRequestStatus $ NoDifferencesStatus mJobUrl
    exitWithInfo "No style differences found"

  logInfo "Restyling produced differences"

  patch <- getRestyledPatch
  withThreadContext ["patch" .= True]
    $ traverse_ (logInfo . (:# []))
    $ T.lines patch

  -- This message only makes sense in the context of a Job
  for_ mJobUrl $ \jobUrl -> do
    logInfo ""
    logInfo "NOTE: you can manually apply these fixes by running:"
    logInfo ""
    logInfo "    git checkout <your branch>"
    logInfo $ "    curl " <> getUrl jobUrl <> "/patch | git am" :# []
    logInfo "    git push"
    logInfo ""

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
    sendPullRequestStatus $ DifferencesStatus mJobUrl
    logInfo
      $ "Not creating Restyle PR"
      :# ["reason" .= ("disabled by config" :: Text)]
    exitWithInfo "Please correct style using the process described above"

  let
    isDangerous =
      pullRequestRepoPublic pullRequest && pullRequestIsFork pullRequest

    dangerDetails :: Text
    dangerDetails =
      "Forks in open source projects could contain unsafe contributions"

  when isDangerous $ do
    sendPullRequestStatus $ DifferencesStatus mJobUrl
    logInfo $ "Not creating Restyle PR" :# ["reason" .= dangerDetails]
    exitWithInfo "Please correct style using the process described above"

  mRestyledPullRequest <- findRestyledPullRequest pullRequest
  url <-
    restyledPullRequestHtmlUrl <$> case mRestyledPullRequest of
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
     , HasLogger env
     , HasHostDirectoryOption env
     , HasImageCleanupOption env
     , HasRestrictions env
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

getRestyledPatch
  :: (MonadGit m, MonadReader env m, HasPullRequest env) => m Text
getRestyledPatch = do
  sha <- pullRequestHeadSha <$> view pullRequestL
  gitFormatPatch $ Just $ unpack sha

getChangedPaths :: MonadGitHub m => PullRequest -> m [FilePath]
getChangedPaths pullRequest = do
  files <-
    runGitHub
      $ pullRequestFilesR
        (pullRequestOwnerName pullRequest)
        (pullRequestRepoName pullRequest)
        (pullRequestNumber pullRequest)
        FetchAll

  pure $ V.toList $ unpack . fileFilename <$> files
