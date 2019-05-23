module Main (main) where

import RIO

import Restyler.App.Class (HasProcess(..), HasSystem(..))
import Restyler.Config (Config(..))
import Restyler.Logger (restylerLogFunc')
import Restyler.Restyler.Run (runRestylers_)
import Restyler.Setup (loadConfig)
import qualified RIO.Directory as Directory
import UnliftIO.Environment (getArgs)
import qualified UnliftIO.Process as Process

newtype App = App
    { appLogFunc :: LogFunc
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasSystem App where
    getCurrentDirectory = Directory.getCurrentDirectory
    setCurrentDirectory = Directory.setCurrentDirectory
    doesFileExist = Directory.doesFileExist
    readFile = readFileUtf8

instance HasProcess App where
    callProcess = Process.callProcess
    readProcess = Process.readProcess

main :: IO ()
main = runRIO loadApp $ do
    config <- loadConfig
    runRestylers_ (cRestylers config) =<< getArgs

loadApp :: App
loadApp = App $ restylerLogFunc' LevelInfo True
