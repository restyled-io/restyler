module Main
  ( main
  ) where

import Restyler.Prelude

import Blammo.Logging.LogSettings.Env qualified as LoggingEnv
import Env qualified
import Restyler.App (runAppT)
import Restyler.Config (loadConfig)
import Restyler.HostDirectoryOption
import Restyler.ImageCleanupOption
import Restyler.ManifestOption
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

instance HasManifestOption App where
  manifestOptionL = optionsL . manifestOptionL

instance HasRestrictions App where
  restrictionsL = optionsL . restrictionsL

instance HasHostDirectoryOption App where
  hostDirectoryOptionL = optionsL . hostDirectoryOptionL

constL :: b -> Lens' a b
constL x = lens (const x) const

instance HasImageCleanupOption App where
  imageCleanupOptionL = constL $ toImageCleanupOption False

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
                , oImageCleanup = False
                }
          }

  runAppT app $ do
    config <- loadConfig
    runRestylers_ config =<< getArgs
