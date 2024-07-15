module Restyler.Opt
  ( -- * Domain options
    PullRequestOption (..)
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

data PullRequestOption = PullRequestOption
  { repo :: Repository
  , number :: Int
  }

-- | TODO @owner/repo#number@
readPR :: String -> Either String PullRequestOption
readPR _ =
  Right
    $ PullRequestOption
      { repo =
          Repository
            { owner = "restyled-io"
            , repo = "restyler"
            }
      , number = 235
      }

optPullRequest :: Parser PullRequestOption
optPullRequest =
  option (eitherReader readPR)
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
