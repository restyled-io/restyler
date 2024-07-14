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
optParser = optRestyleJob <|> optRestyleGHA <|> optRestyleLocal

optRestyleJob :: Parser Command
optRestyleJob =
  go
    <$> optPR
    <*> option (eitherReader readURL) (long "job-url")
 where
  go :: PR -> URL -> Command
  go pr = RestyleJob pr.repo pr.number

optRestyleGHA :: Parser Command
optRestyleGHA = go <$> optPR
 where
  go :: PR -> Command
  go pr = RestyleGHA pr.repo pr.number

optRestyleLocal :: Parser Command
optRestyleLocal = RestyleLocal <$> some1 (argument str $ metavar "PATH")

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
optPR = option (eitherReader readPR) $ long "pr"

newtype URL = URL URI

readURL :: String -> Either String URL
readURL x =
  URL <$> note ("Not a valid absolute URI: " <> x) (parseAbsoluteURI x)
