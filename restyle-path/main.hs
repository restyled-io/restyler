module Main (main) where

import RIO

import Conduit (runResourceT, sinkFile)
import Data.Text (unpack)
import Network.HTTP.Simple hiding (Request)
import Restyler.App.Class (HasDownloadFile(..), HasProcess(..), HasSystem(..))
import Restyler.Config (Config(..), loadConfig)
import Restyler.Options
import Restyler.Restyler.Run (runRestylers_)
import qualified RIO.Directory as Directory
import UnliftIO.Environment (getArgs, lookupEnv)
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

instance HasProcess App where
    callProcess = Process.callProcess
    readProcess = Process.readProcess

instance HasDownloadFile App where
    downloadFile url path = liftIO $ do
        request <- parseRequest $ unpack url
        runResourceT $ httpSink request $ \_ -> sinkFile path

main :: IO ()
main = do
    mDir <- lookupEnv "HOST_DIRECTORY"
    verbose <- maybe False (/= "") <$> lookupEnv "DEBUG"
    options <- setupLog <$> logOptionsHandle stdout verbose
    withLogFunc options $ \logFunc -> runRIO (setupApp logFunc mDir) $ do
        config <- loadConfig
        runRestylers_ (cRestylers config) =<< getArgs
  where
    setupLog = setLogUseTime False . setLogUseLoc False
    setupApp logFunc mDir = App
        { appLogFunc = logFunc
        , appOptions = Options
            { oAccessToken = error "unused"
            , oLogLevel = error "unused"
            , oLogColor = error "unused"
            , oOwner = error "unused"
            , oRepo = error "unused"
            , oPullRequest = error "unused"
            , oJobUrl = error "unused"
            , oHostDirectory = mDir
            }
        }
