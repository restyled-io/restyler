module Restyler.Main
    ( restylerMain
    ) where

import Restyler.Prelude

import Restyler.App.Error
import Restyler.Capabilities.DownloadFile
import Restyler.Capabilities.Git
import Restyler.Capabilities.GitHub
import Restyler.Capabilities.Hushed
import Restyler.Capabilities.Logger
import Restyler.Capabilities.Process
import Restyler.Capabilities.System
import Restyler.Comment
import Restyler.Config
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequest.Status
import Restyler.RestyledPullRequest
import Restyler.Restyler.Run
import Restyler.RestylerResult

restylerMain
    :: ( MonadLogger m
       , MonadHushed m
       , MonadSystem m
       , MonadProcess m
       , MonadDownloadFile m
       , MonadGitHub m
       , MonadGit m
       , MonadError AppError m
       , MonadReader env m
       , HasOptions env
       , HasPullRequest env
       , HasConfig env
       )
    => m ()
restylerMain = do
    mRestyledPullRequest <- findRestyledPullRequest

    result <- do
        eitherM
                (pure . NotRestyled)
                (const $ do
                    checkoutRestyleBranch mRestyledPullRequest
                    restyle
                )
            $ runExceptT gateShouldNotRestyle

    case result of
        NotRestyled reason -> do
            traverse_ cleanupRestyledPullRequest mRestyledPullRequest
            logInfo $ display reason
        RestyledNoDifferences{} -> do
            traverse_ cleanupRestyledPullRequest mRestyledPullRequest
            sendPullRequestStatus NoDifferencesStatus
            logInfo "No style differences found"
        Restyled results -> do
            mUrl <- openOrUpdateRestyledPullRequest results mRestyledPullRequest
            sendPullRequestStatus $ DifferencesStatus mUrl
            logInfo "Restyling successful"

data RestyleResult
    = NotRestyled NotRestyledReason
    | RestyledNoDifferences [RestylerResult]
    | Restyled [RestylerResult]

instance Display RestyleResult where
    display = \case
        NotRestyled reason -> "Not restyled: " <> display reason
        RestyledNoDifferences{} -> "No differences"
        Restyled results -> "Restyled: " <> displayIntercalated "," results

data NotRestyledReason
    = NotRestyledPullRequestClosed
    | NotRestyledSkipped NotRestyledSkippedReason

instance Display NotRestyledReason where
    display = \case
        NotRestyledPullRequestClosed -> "Pull Request closed"
        NotRestyledSkipped reason -> "Skipped, " <> display reason

data NotRestyledSkippedReason
    = NotRestyledConfigDisabled
    | NotRestyledSkippedIgnoreLabels

instance Display NotRestyledSkippedReason where
    display = \case
        NotRestyledConfigDisabled -> "disabled in configuration"
        NotRestyledSkippedIgnoreLabels -> "Labels matched ignores"

gateShouldNotRestyle
    :: ( MonadLogger m
       , MonadGitHub m
       , MonadError NotRestyledReason m
       , MonadReader env m
       , HasPullRequest env
       , HasConfig env
       )
    => m ()
gateShouldNotRestyle = do
    pullRequest <- view pullRequestL
    when (pullRequestIsClosed pullRequest)
        $ throwError NotRestyledPullRequestClosed

    whenConfig (not . cEnabled) $ throwError $ NotRestyledSkipped
        NotRestyledConfigDisabled

    labels <- getPullRequestLabelNames pullRequest
    whenConfig ((labels `intersects`) . cIgnoreLabels)
        $ throwError
        $ NotRestyledSkipped NotRestyledSkippedIgnoreLabels

checkoutRestyleBranch
    :: (MonadLogger m, MonadGit m, MonadReader env m, HasPullRequest env)
    => Maybe RestyledPullRequest
    -> m ()
checkoutRestyleBranch mRestyledPullRequest = do
    ref <- maybeM
        (pullRequestRestyledHeadRef <$> view pullRequestL)
        (pure . restyledPullRequestHeadRef)
        (pure mRestyledPullRequest)

    logInfo $ "Checking out " <> display ref
    gitCheckout $ unpack ref

restyle
    :: ( MonadLogger m
       , MonadHushed m
       , MonadSystem m
       , MonadProcess m
       , MonadDownloadFile m
       , MonadGit m
       , MonadError AppError m
       , MonadReader env m
       , HasOptions env
       , HasPullRequest env
       , HasConfig env
       )
    => m RestyleResult
restyle = do
    config <- view configL
    pullRequest <- view pullRequestL
    pullRequestPaths <- changedPaths $ pullRequestBaseRef pullRequest
    results <- runRestylers config pullRequestPaths
    let headRef = pullRequestLocalHeadRef pullRequest
    differences <- not . null <$> changedPaths headRef
    pure $ if differences
        then Restyled results
        else RestyledNoDifferences results

openOrUpdateRestyledPullRequest
    :: ( MonadLogger m
       , MonadGitHub m
       , MonadGit m
       , MonadReader env m
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       )
    => [RestylerResult]
    -> Maybe RestyledPullRequest
    -> m (Maybe URL)
openOrUpdateRestyledPullRequest results mRestyledPullRequest = do
    config <- view configL

    if cPullRequests config
        then Just . pullRequestHtmlUrl <$> case mRestyledPullRequest of
            Nothing -> do
                pullRequest <- view pullRequestL
                createRestyledPullRequest pullRequest results
            Just pr -> updateRestyledPullRequest pr results
        else oJobUrl <$> view optionsL

cleanupRestyledPullRequest
    :: (MonadGitHub m, MonadReader env m, HasPullRequest env)
    => RestyledPullRequest
    -> m ()
cleanupRestyledPullRequest restyledPullRequest = do
    pullRequest <- view pullRequestL
    clearRestyledComments pullRequest
    closeRestyledPullRequest restyledPullRequest

changedPaths :: MonadGit m => Text -> m [FilePath]
changedPaths branch = do
    ref <- maybe branch pack <$> gitMergeBase (unpack branch)
    gitDiffNameOnly $ Just $ unpack ref
