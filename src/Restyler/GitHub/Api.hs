{-# LANGUAGE UndecidableInstances #-}

module Restyler.GitHub.Api
  ( -- * Domain-specific constructs
    getPullRequest
  , getPullRequestFiles

    -- * "GitHub" dependency-injection
  , MonadGitHub (..)

    -- * @DerivingVia@
  , GitHubToken (..)
  , envGitHubToken
  , HasGitHubToken (..)
  , ActualGitHub (..)
  , GitHubError (..)
  ) where

import Restyler.Prelude

import Data.Aeson (Value, decodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Env qualified
import GitHub (github)
import GitHub qualified
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Simple (getResponseStatus)
import Network.HTTP.Types.Status (Status, statusCode)
import Restyler.AnnotatedException (checkpointCallStack, handleTo, throw)
import Restyler.GitHub.PullRequest
import Restyler.GitHub.PullRequest.File
import Restyler.Options.Repository
import System.IO.Error (userError)

getPullRequest
  :: (MonadUnliftIO m, MonadGitHub m)
  => RepositoryOption
  -> Int
  -> m PullRequest
getPullRequest repo pr = do
  labels <-
    fromGitHubVector (Right . convertLabel)
      =<< ghLabelsOnIssue ghOwner ghRepo ghIssueId

  fromGitHub (Right . convertPullRequest labels)
    =<< ghPullRequest ghOwner ghRepo ghNumber
 where
  ghOwner = GitHub.mkOwnerName repo.owner
  ghRepo = GitHub.mkRepoName repo.repo
  ghNumber = GitHub.IssueNumber pr
  ghIssueId = GitHub.mkId Proxy pr

getPullRequestFiles
  :: (MonadUnliftIO m, MonadGitHub m)
  => RepositoryOption
  -> Int
  -> m [PullRequestFile]
getPullRequestFiles repo pr = do
  fromGitHubVector convertFile
    =<< ghPullRequestFiles ghOwner ghRepo ghNumber
 where
  ghOwner = GitHub.mkOwnerName repo.owner
  ghRepo = GitHub.mkRepoName repo.repo
  ghNumber = GitHub.IssueNumber pr

convertLabel :: GitHub.IssueLabel -> Label
convertLabel gh = Label {name = GitHub.untagName $ GitHub.labelName gh}

convertPullRequest
  :: [Label]
  -- ^ Separately converted 'Label's
  --
  -- The "GitHub" library doens't parse the @labels@ field of @PullRequest@, so
  -- we separately call @labelsOnIssueR@ for now and pass them in.
  -> GitHub.PullRequest
  -> PullRequest
convertPullRequest labels gh =
  PullRequest
    { html_url = GitHub.pullRequestHtmlUrl gh
    , number = GitHub.unIssueNumber $ GitHub.pullRequestNumber gh
    , title = GitHub.pullRequestTitle gh
    , user = convertUser $ GitHub.pullRequestUser gh
    , state = convertState $ GitHub.pullRequestState gh
    , labels = labels
    , head = convertCommit $ GitHub.pullRequestHead gh
    , base = convertCommit $ GitHub.pullRequestBase gh
    }

convertUser :: GitHub.SimpleUser -> User
convertUser gh = User {login = GitHub.untagName $ GitHub.simpleUserLogin gh}

convertState :: GitHub.IssueState -> PullRequestState
convertState = \case
  GitHub.StateOpen -> PullRequestOpen
  GitHub.StateClosed -> PullRequestClosed

convertCommit :: GitHub.PullRequestCommit -> Commit
convertCommit gh =
  Commit
    { ref = GitHub.pullRequestCommitRef gh
    , sha = GitHub.pullRequestCommitSha gh
    , repo = convertRepo $ GitHub.pullRequestCommitRepo gh
    }

convertRepo :: Maybe GitHub.Repo -> Repo
convertRepo = \case
  Nothing -> error "unexpected: PR had no repo in head or base"
  Just gh ->
    Repo
      { name = GitHub.untagName $ GitHub.repoName gh
      , owner = convertOwner $ GitHub.repoOwner gh
      , private = GitHub.repoPrivate gh
      }

convertOwner :: GitHub.SimpleOwner -> Owner
convertOwner gh =
  Owner
    { login = GitHub.untagName $ GitHub.simpleOwnerLogin gh
    }

convertFile :: GitHub.File -> Either String PullRequestFile
convertFile gh = do
  status <- pullRequestFileStatusFromText $ GitHub.fileStatus gh
  pure PullRequestFile {filename = unpack $ GitHub.fileFilename gh, status}

fromGitHubVector
  :: (MonadUnliftIO m, Foldable t)
  => (a -> Either String b)
  -> Either GitHub.Error (t a)
  -> m [b]
fromGitHubVector f =
  either (throw . userError) pure <=< either throw (pure . traverse f . toList)

fromGitHub
  :: MonadUnliftIO m => (a -> Either String b) -> Either GitHub.Error a -> m b
fromGitHub f =
  either (throw . userError) pure <=< either throw (pure . f)

-- | A thin seam over "GitHub"
--
-- Each method should map to a @whateverR@ function and accept and return
-- arguments in "GitHub" types. We want the logic on top (combining and
-- modifying responses into our domain types) to be testable, and thus
-- implemented on top of this, not within.
class Monad m => MonadGitHub m where
  ghLabelsOnIssue
    :: HasCallStack
    => GitHub.Name GitHub.Owner
    -> GitHub.Name GitHub.Repo
    -> GitHub.Id GitHub.Issue
    -> m (Either GitHub.Error (Vector GitHub.IssueLabel))

  ghPullRequest
    :: HasCallStack
    => GitHub.Name GitHub.Owner
    -> GitHub.Name GitHub.Repo
    -> GitHub.IssueNumber
    -> m (Either GitHub.Error GitHub.PullRequest)

  ghPullRequestFiles
    :: HasCallStack
    => GitHub.Name GitHub.Owner
    -> GitHub.Name GitHub.Repo
    -> GitHub.IssueNumber
    -> m (Either GitHub.Error (Vector GitHub.File))

newtype GitHubToken = GitHubToken
  { unwrap :: Text
  }
  deriving newtype (IsString)

envGitHubToken :: Env.Parser Env.Error GitHubToken
envGitHubToken =
  Env.var Env.str "GITHUB_TOKEN"
    $ Env.help "GitHub token with access to the repo and PR"

githubTokenGitHubAuth :: GitHubToken -> GitHub.Auth
githubTokenGitHubAuth = GitHub.OAuth . encodeUtf8 . (.unwrap)

class HasGitHubToken a where
  getGitHubToken :: a -> GitHubToken

instance HasGitHubToken GitHubToken where
  getGitHubToken = id

newtype ActualGitHub m a = ActualGitHub
  { unwrap :: m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadReader env
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    )

instance
  (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasGitHubToken env)
  => MonadGitHub (ActualGitHub m)
  where
  ghLabelsOnIssue owner repo number = checkpointCallStack $ do
    logDebug
      $ "GitHub.labelsOnIssueR"
      :# ["owner" .= owner, "repo" .= repo, "number" .= number]
    runGitHub $ GitHub.labelsOnIssueR owner repo number GitHub.FetchAll

  ghPullRequest owner repo number = checkpointCallStack $ do
    logDebug
      $ "GitHub.pullRequestR"
      :# ["owner" .= owner, "repo" .= repo, "number" .= number]
    runGitHub $ GitHub.pullRequestR owner repo number

  ghPullRequestFiles owner repo number = checkpointCallStack $ do
    logDebug
      $ "GitHub.pullRequestFilesR"
      :# ["owner" .= owner, "repo" .= repo, "number" .= number]
    runGitHub $ GitHub.pullRequestFilesR owner repo number GitHub.FetchAll

runGitHub
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasGitHubToken env
     , GitHub.GitHubRW req (IO b)
     )
  => req
  -> m b
runGitHub req = handleTo toGitHubError $ do
  auth <- asks $ githubTokenGitHubAuth . getGitHubToken
  liftIO $ github auth req

data GitHubError
  = GitHubHTTPError GitHub.Error ByteString Status ByteString
  | GitHubError GitHub.Error
  deriving stock (Show)

instance Exception GitHubError where
  displayException = \case
    GitHubHTTPError _ path status body ->
      unpack
        $ "GitHub request for "
        <> decodeUtf8 @Text path
        <> " responded "
        <> show @Text (statusCode status)
        <> "\n"
        <> decodeUtf8 (tryEncodePretty body)
    GitHubError ex -> displayException ex

toGitHubError :: GitHub.Error -> GitHubError
toGitHubError = \case
  ex@(GitHub.HTTPError (HttpExceptionRequest req (StatusCodeException resp body))) ->
    GitHubHTTPError ex (HTTP.path req) (getResponseStatus resp) body
  ex -> GitHubError ex

tryEncodePretty :: ByteString -> ByteString
tryEncodePretty bs = maybe bs (toStrict . encodePretty @Value) $ decodeStrict bs
