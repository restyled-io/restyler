module Restyler.Opt
  ( -- * Domain options
    PullRequestOption (..)
  , readPullRequest
  , optPullRequest
  , URL
  , optJobURL

    -- * Re-exports
  , module Options.Applicative

    -- * Extensions
  , parse
  )
where

import Restyler.Prelude hiding (URL (..))

import Network.URI (URI, parseAbsoluteURI)
import Options.Applicative
import Restyler.GitHub.Repository
import Restyler.ReadP

data PullRequestOption = PullRequestOption
  { repo :: Repository
  , number :: Int
  }
  deriving stock (Eq, Show)

readPullRequest :: String -> Either String PullRequestOption
readPullRequest =
  parseReadP
    $ PullRequestOption
    <$> ( Repository
            <$> textTill1 '/'
            <*> textTill1 '#'
        )
    <*> digits

optPullRequest :: Parser PullRequestOption
optPullRequest =
  option (eitherReader readPullRequest)
    $ long "pr"
    <> metavar "OWNER/REPO#NUMBER"
    <> help "Pull Request to restyler"

newtype URL = URL URI

readURL :: String -> Either String URL
readURL x =
  URL <$> note ("Not a valid absolute URI: " <> x) (parseAbsoluteURI x)

optJobURL :: Parser URL
optJobURL =
  option (eitherReader readURL)
    $ long "job-url"
    <> metavar "URL"
    <> help "Triggering Restyled Job's URL"

parse :: String -> Parser a -> IO a
parse d p = execParser $ info (p <**> helper) $ fullDesc <> progDesc d
