module Restyler.Command () where

{-
  ( Command (..)
  , getCommand
  ) where

import Restyler.Prelude hiding (URL (..))

import Data.List.NonEmpty (some1)
import Network.URI (URI, parseAbsoluteURI)
import Options.Applicative
import Restyler.GitHub.Repository
import Restyler.Options.RestyleGHA qualified as RestyleGHA
import Restyler.Options.RestyleLocal qualified as RestyleLocal

data Command
  = -- | @restyle --pr "owner/repo#number" --job-url ...@
    --
    -- - Clone
    -- - Run @RestyleGHA@
    -- - Emit status
    RestyleJob RestyleLocal.Options Repository Int URL
  | -- | @restyle --pr "owner/repo#number"
    --
    -- - Fetch PR details
    -- - Check closed, ignore, etc
    -- - Run @RestyleLocal@ with commits
    RestyleGHA RestyleGHA.EnvOptions RestyleLocal.Options Repository Int
  | -- | @restyle path...@
    --
    -- - Run restylers with or without commits
    RestyleLocal RestyleLocal.Options (NonEmpty FilePath)

getCommand :: IO Command
getCommand = do
  env <- RestyleLocal.envOptions
  cmd <- execParser $ info (optParser <**> helper) fullDesc

  case cmd of
    Job opt repo pr url -> pure $ RestyleJob opt repo pr url
    GHA opt repo pr ->
      RestyleGHA
        <$> RestyleGHA.envOptions
        <*> pure (env <> opt)
        <*> pure repo
        <*> pure pr
    Local opt paths -> pure $ RestyleLocal (env <> opt) paths

data Cmd
  = Job RestyleLocal.Options Repository Int URL
  | GHA RestyleLocal.Options Repository Int
  | Local RestyleLocal.Options (NonEmpty FilePath)

optParser :: Parser Cmd
optParser =
  subparser
    $ mconcat
      [ command "io" $ info' optJob "Restyle on restyled.io"
      , command "gh" $ info' optGHA "Restyle on GitHub Actions"
      , command "fs" $ info' optLocal "Restyle local files"
      ]

info' :: Parser a -> String -> ParserInfo a
info' p d = info (p <**> helper) $ fullDesc <> progDesc d

optJob :: Parser Cmd
optJob = go <$> RestyleLocal.optParser <*> optPR <*> optJobURL
 where
  go :: RestyleLocal.Options -> PR -> URL -> Cmd
  go opt pr = Job opt pr.repo pr.number

optGHA :: Parser Cmd
optGHA =

optLocal :: Parser Cmd
optLocal =
  Local
    <$> RestyleLocal.optParser
    <*> some1 (argument str $ metavar "PATH")

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
  -}
