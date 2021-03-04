{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module Restyler.Capabilities.Process.Mock
    ( MockProcess(..)
    , ProcessMocks
    , processMocks
    , HasProcessMocks(..)
    , prependProcessMock
    ) where

import Restyler.Prelude

import Control.Lens ((%=))
import Control.Monad.State
import Prelude (userError)
import Restyler.App.Error
import Restyler.Capabilities.Process

newtype MockProcess m a = MockProcess
    { unMockProcess :: m a
    }
    deriving newtype (Applicative, Functor, Monad, MonadError e, MonadState s)

instance (MonadError AppError m, MonadState env m, HasProcessMocks env)
    => MonadProcess (MockProcess m) where
    callProcess cmd args = matchProcess cmd args Nothing >>= \case
        (ExitFailure ec, _out) -> throwExitCode ec
        (_, _) -> pure ()

    callProcessExitCode cmd args = matchProcess cmd args Nothing >>= \case
        (ec, _) -> pure ec

    readProcess cmd args in_ = matchProcess cmd args (Just in_) >>= \case
        (ExitFailure ec, _out) -> throwExitCode ec
        (_, out) -> pure out

throwExitCode :: MonadError AppError m => Int -> m a
throwExitCode ec =
    throwError $ SystemError $ userError $ "Process failed: " <> show ec

data ProcessMock = ProcessMock
    { pmCmd :: String
    , pmMatchArgs :: [String] -> Bool
    , pmMatchStdin :: Maybe String -> Bool
    , pmResult :: Either IOException (ExitCode, String)
    }

processMockMatches :: String -> [String] -> Maybe String -> ProcessMock -> Bool
processMockMatches cmd args mStdin ProcessMock {..} =
    and [pmCmd == cmd, pmMatchArgs args, pmMatchStdin mStdin]

matchProcess
    :: (MonadError AppError m, MonadState env m, HasProcessMocks env)
    => String
    -> [String]
    -> Maybe String
    -> m (ExitCode, String)
matchProcess cmd args mStdin = do
    ProcessMocks mocks <- gets $ view processMocksL

    let meMatch = pmResult <$> find (processMockMatches cmd args mStdin) mocks

    case meMatch of
        Nothing -> errorNoMock cmd args mStdin
        Just (Left ex) -> throwError $ SystemError ex
        Just (Right x) -> pure x

errorNoMock :: String -> [String] -> Maybe String -> a
errorNoMock cmd args mStdin =
    error
        $ "No ProcessMock found for "
        <> cmd
        <> ", matching arguments "
        <> show args
        <> maybe "" (", with stdin: " <>) mStdin

newtype ProcessMocks = ProcessMocks
    { unProcessMocks :: [ProcessMock]
    }

unL :: Lens' ProcessMocks [ProcessMock]
unL = lens unProcessMocks $ \x y -> x { unProcessMocks = y }

processMocks :: ProcessMocks
processMocks = ProcessMocks []

class HasProcessMocks env where
    processMocksL :: Lens' env ProcessMocks

instance HasProcessMocks ProcessMocks where
    processMocksL = id

prependProcessMock
    :: (MonadState env m, HasProcessMocks env)
    => String
    -- ^ Command
    -> ([String] -> Bool)
    -- ^ Expected arguments
    -> (Maybe String -> Bool)
    -- ^ Expected stdin
    -> Either IOException (ExitCode, String)
    -> m ()
prependProcessMock pmCmd pmMatchArgs pmMatchStdin pmResult =
    processMocksL . unL %= (mock :)
    where mock = ProcessMock { pmCmd, pmMatchArgs, pmMatchStdin, pmResult }
