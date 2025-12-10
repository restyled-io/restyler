-- |
--
-- Module      : Restyler.Config.RunUser
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.RunUser
  ( RunUser
  , runUserArg
  , runUserShExec
  , HasRunUser (..)
  , runUserParser
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import OptEnvConf
import Restyler.AnnotatedException (throw)
import System.Process.Typed

data RunUser = RunUser
  { uid :: Int
  , gid :: Int
  }
  deriving stock (Eq, Show)

runUserArg :: RunUser -> String
runUserArg ug = show ug.uid <> ":" <> show ug.gid

runUserShExec :: RunUser -> [String] -> String
runUserShExec ug args =
  intercalate
    " && "
    [ "groupadd -g " <> show ug.gid <> " app"
    , "useradd -u " <> show ug.uid <> " -g " <> show ug.gid <> " -m app"
    , "exec su -g "
        <> show ug.gid
        <> " -p app"
        <> concatMap (\arg -> " \"" <> arg <> "\"") args -- TODO: naive
    ]

class HasRunUser env where
  getRunUser :: env -> Maybe RunUser

runUserParser :: Parser (Maybe RunUser)
runUserParser =
  mapIO
    ( \addUser ->
        if addUser
          then Just <$> lookupRunUser
          else pure Nothing
    )
    $ withDefault
      True
      ( yesNoSwitch
          [ help "Include `--run-user \"$(id -u):$(id -g)\"' in docker-run"
          , name "run-user"
          ]
      )

lookupRunUser :: IO RunUser
lookupRunUser =
  RunUser
    <$> readIdOnlyStdout_ 'u'
    <*> readIdOnlyStdout_ 'g'

data ReadIdOnlyError = ReadIdOnlyError
  { opt :: Char
  , output :: Text
  }
  deriving stock (Show)

instance Exception ReadIdOnlyError where
  displayException ex =
    "Output from `id -"
      <> [ex.opt]
      <> "` was not parsable as an Int: "
      <> show ex.output
      <> ". Tip: use --no-user to avoid this step."

readIdOnlyStdout_ :: Char -> IO Int
readIdOnlyStdout_ opt = do
  t <- decodeUtf8 <$> readProcessStdout_ (proc "id" ['-' : [opt]])

  maybe (throw $ ReadIdOnlyError opt t) pure
    $ readMaybe
    $ unpack
    $ T.dropWhileEnd isSpace t
