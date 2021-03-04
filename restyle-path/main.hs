module Main
    ( main
    ) where

import Restyler.Prelude

import qualified Env
import qualified Restyler.App.Startup as Startup
import Restyler.AppT
import Restyler.CLI
import Restyler.Config
import Restyler.Options (Options(..))
import Restyler.Restyler.Run (runRestylers_)
import UnliftIO.Environment (getArgs)

data EnvOptions = EnvOptions
    { eoHostDirectory :: Maybe FilePath
    , eoVerbose :: Bool
    , eoUnrestricted :: Bool
    }

-- brittany-disable-next-binding

envParser :: Env.Parser Env.Error EnvOptions
envParser = EnvOptions
    <$> optional (Env.var Env.str "HOST_DIRECTORY" Env.keep)
    <*> Env.switch "DEBUG" Env.keep
    <*> Env.switch "UNRESTRICTED" Env.keep

main :: IO ()
main = restylerCLI parseOptions $ \options _ -> do
    let app = Startup.loadApp options "."

    runAppT app $ do
        config <- loadConfig
        runRestylers_ config =<< getArgs

parseOptions :: IO Options
parseOptions = do
    EnvOptions {..} <- Env.parse id envParser
    pure Options
        { oAccessToken = error "unused"
        , oLogLevel = if eoVerbose then LevelDebug else LevelInfo
        , oLogColor = True -- TODO: istty
        , oOwner = error "unused"
        , oRepo = error "unused"
        , oPullRequest = error "unused"
        , oJobUrl = error "unused"
        , oHostDirectory = eoHostDirectory
        , oUnrestricted = eoUnrestricted
        }
