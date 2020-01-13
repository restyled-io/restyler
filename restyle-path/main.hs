module Main (main) where

import RIO

import Conduit (runResourceT, sinkFile)
import Data.Text (unpack)
import qualified Env
import Network.HTTP.Simple hiding (Request)
import Restyler.App.Class (HasDownloadFile(..), HasProcess(..), HasSystem(..))
import Restyler.Config (loadConfig)
import Restyler.Options
import Restyler.Restyler.Run (runRestylers_)
import qualified RIO.Directory as Directory
import UnliftIO.Environment (getArgs)
import qualified UnliftIO.Process as Process

data App = App
    { appLogFunc :: LogFunc
    , appOptions :: Options
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasOptions App where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasSystem App where
    getCurrentDirectory = Directory.getCurrentDirectory
    setCurrentDirectory = Directory.setCurrentDirectory
    doesFileExist = Directory.doesFileExist
    readFile = readFileUtf8
    readFileBS = readFileBinary
    writeFile = writeFileUtf8

instance HasProcess App where
    callProcess = Process.callProcess
    callProcessExitCode cmd args = Process.withCreateProcess proc
        $ \_ _ _ p -> Process.waitForProcess p
        where proc = (Process.proc cmd args) { Process.delegate_ctlc = True }
    readProcess = Process.readProcess

instance HasDownloadFile App where
    downloadFile url path = liftIO $ do
        request <- parseRequestThrow $ unpack url
        runResourceT $ httpSink request $ \_ -> sinkFile path

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
main = do
    envOptions@EnvOptions {..} <- Env.parse id envParser
    options <- setupLog <$> logOptionsHandle stdout eoVerbose
    withLogFunc options $ \logFunc -> runRIO (setupApp logFunc envOptions) $ do
        config <- loadConfig
        runRestylers_ config =<< getArgs
  where
    setupLog = setLogUseTime False . setLogUseLoc False
    setupApp logFunc EnvOptions {..} = App
        { appLogFunc = logFunc
        , appOptions = Options
            { oAccessToken = error "unused"
            , oLogLevel = error "unused"
            , oLogColor = error "unused"
            , oOwner = error "unused"
            , oRepo = error "unused"
            , oPullRequest = error "unused"
            , oJobUrl = error "unused"
            , oHostDirectory = eoHostDirectory
            , oUnrestricted = eoUnrestricted
            }
        }
