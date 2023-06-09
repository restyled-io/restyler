module Main
  ( main
  ) where

import Restyler.Prelude

import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import qualified Env
import Restyler.App (runAppT)
import Restyler.Config (loadConfig)
import Restyler.Options
import Restyler.Restrictions
import Restyler.Restyler.Run (runRestylers_)

data App = App
  { appLogger :: Logger
  , appOptions :: Options
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

instance HasOptions App where
  optionsL = lens appOptions $ \x y -> x {appOptions = y}

data EnvOptions = EnvOptions
  { eoLogSettings :: LogSettings
  , eoHostDirectory :: Maybe FilePath
  , eoManifest :: Maybe FilePath
  , eoRestrictions :: Restrictions
  }

-- brittany-disable-next-binding

envParser :: Env.Parser Env.Error EnvOptions
envParser =
  EnvOptions
    <$> LoggingEnv.parser
    <*> optional (Env.var Env.str "HOST_DIRECTORY" mempty)
    <*> optional (Env.var Env.str "MANIFEST" mempty)
    <*> envRestrictions

main :: IO ()
main = do
  EnvOptions {..} <- Env.parse id envParser
  logger <- newLogger eoLogSettings

  let app =
        App
          { appLogger = logger
          , appOptions =
              Options
                { oAccessToken = error "unused"
                , oLogSettings = eoLogSettings
                , oOwner = error "unused"
                , oRepo = error "unused"
                , oPullRequest = error "unused"
                , oManifest = eoManifest
                , oJobUrl = error "unused"
                , oHostDirectory = eoHostDirectory
                , oRepoDisabled = False
                , oPlanRestriction = Nothing
                , oPlanUpgradeUrl = Nothing
                , oRestrictions = eoRestrictions
                , oStatsdHost = Nothing
                , oStatsdPort = Nothing
                }
          }

  runAppT app $ do
    config <- loadConfig
    runRestylers_ config =<< getArgs
