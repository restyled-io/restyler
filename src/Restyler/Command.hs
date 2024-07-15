module Restyler.Command
  ( Command (..)
  , getCommand
  ) where

import Restyler.Prelude hiding (URL (..))

import Data.List.NonEmpty (some1)
import Network.URI (URI, parseAbsoluteURI)
import Options.Applicative
import Restyler.GitHub.Repository

data Command
  = -- | @restyle --pr "owner/repo#number" --job-url ...@
    --
    -- - Clone
    -- - Run @RestyleGHA@
    -- - Emit status
    RestyleJob Repository Int URL
  | -- | @restyle --pr "owner/repo#number"
    --
    -- - Fetch PR details
    -- - Check closed, ignore, etc
    -- - Run @RestyleLocal@ with commits
    RestyleGHA Repository Int
  | -- | @restyle path...@
    --
    -- - Run restylers with or without commits
    RestyleLocal (NonEmpty FilePath)

getCommand :: MonadIO m => m Command
getCommand = liftIO $ execParser $ info (optParser <**> helper) fullDesc

optParser :: Parser Command
optParser = optRestyleJobOrGHA <|> optRestyleLocal

optRestyleJobOrGHA :: Parser Command
optRestyleJobOrGHA = go <$> optPR <*> optional optJobURL
 where
  go :: PR -> Maybe URL -> Command
  go pr = \case
    Nothing -> RestyleGHA pr.repo pr.number
    Just url -> RestyleJob pr.repo pr.number url

optRestyleLocal :: Parser Command
optRestyleLocal = RestyleLocal <$> some1 (argument str $ metavar "PATH")

optJobURL :: Parser URL
optJobURL =
  option (eitherReader readURL)
    $ long "job-url"
    <> metavar "URL"
    <> help "Triggering Restyled Job's URL"

data PR = PR
  { repo :: Repository
  , number :: Int
  }

-- | @owner/repo#number@
readPR :: String -> Either String PR
readPR _ =
  Right
    $ PR
      { repo =
          Repository
            { owner = "restyled-io"
            , repo = "restyler"
            }
      , number = 235
      }

optPR :: Parser PR
optPR =
  option (eitherReader readPR)
    $ long "pr"
    <> metavar "OWNER/REPO#NUMBER"
    <> help "Pull Request to restyler"

newtype URL = URL URI

readURL :: String -> Either String URL
readURL x =
  URL <$> note ("Not a valid absolute URI: " <> x) (parseAbsoluteURI x)
