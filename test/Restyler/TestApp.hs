{-# LANGUAGE DerivingVia #-}

module Restyler.TestApp
    ( TestApp
    , TestAppEnv
    , TestAppState
    , runTestApp
    , runTestAppWith
    )
where

import Restyler.Prelude

import Control.Monad.State
import GitHub.Data (IssueNumber(..))
import Restyler.App.Error
import Restyler.Capabilities.DownloadFile
import Restyler.Capabilities.DownloadFile.Staged
import Restyler.Capabilities.Hushed
import Restyler.Capabilities.Hushed.Except
import Restyler.Capabilities.Logger
import Restyler.Capabilities.Logger.Null
import Restyler.Capabilities.Process
import Restyler.Capabilities.Process.Mock
import Restyler.Capabilities.System
import Restyler.Capabilities.System.State
import Restyler.Options
import Test.Hspec.Expectations.Lifted (expectationFailure)

newtype TestAppEnv = TestAppEnv
    { taeOptions :: Options
    }

instance HasOptions TestAppEnv where
    optionsL = lens taeOptions $ \x y -> x { taeOptions = y }

testAppEnv :: TestAppEnv
testAppEnv = TestAppEnv
    { taeOptions = Options
        { oAccessToken = "token"
        , oLogLevel = LevelError
        , oLogColor = False
        , oOwner = "owner"
        , oRepo = "repo"
        , oPullRequest = IssueNumber 1
        , oJobUrl = Nothing
        , oHostDirectory = Nothing
        , oUnrestricted = False
        }
    }

data TestAppState = TestAppState
    { tesFS :: FS
    , tasProcessMocks :: ProcessMocks
    , tasStagedDownloadFiles :: StagedDownloadFiles
    }

instance HasFS TestAppState where
    fsL = lens tesFS $ \x y -> x { tesFS = y }

instance HasProcessMocks TestAppState where
    processMocksL = lens tasProcessMocks $ \x y -> x { tasProcessMocks = y }

instance HasStagedDownloadFiles TestAppState where
    stagedDownloadFilesL =
        lens tasStagedDownloadFiles $ \x y -> x { tasStagedDownloadFiles = y }

newtype TestApp a = TestApp
    { unTestApp :: ReaderT TestAppEnv (StateT TestAppState (ExceptT AppError IO)) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader TestAppEnv
        , MonadState TestAppState
        , MonadError AppError
        )
    deriving MonadLogger via (NullLogger TestApp)
    deriving MonadHushed via (ExceptHushed TestApp)
    deriving MonadSystem via (StateSystem TestApp)
    deriving MonadProcess via (MockProcess TestApp)
    deriving MonadDownloadFile via (StagedDownloadFile TestApp)

runTestApp :: HasCallStack => TestApp a -> IO ()
runTestApp = runTestAppWith testAppEnv

runTestAppWith :: HasCallStack => TestAppEnv -> TestApp a -> IO ()
runTestAppWith env f =
    assertNoAppError
        $ runExceptT
        $ evalStateT (runReaderT (unTestApp f) env)
        $ TestAppState
              { tesFS = buildFS
              , tasProcessMocks = processMocks
              , tasStagedDownloadFiles = stagedDownloadFiles
              }

assertNoAppError :: (HasCallStack, MonadIO m) => m (Either AppError a) -> m ()
assertNoAppError f = do
    result <- f

    case result of
        Left err -> expectationFailure $ unlines
            [ "runTestApp resulted in an AppError:"
            , replicate 80 '-'
            , prettyAppError err
            , replicate 80 '-'
            , "If this is expected, use catchError and assert on it."
            ]
        Right _ -> pure ()
