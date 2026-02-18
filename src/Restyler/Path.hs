-- |
--
-- Module      : Restyler.Path
-- Copyright   : (c) 2026 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Path
  ( SomePath (..)
  , somePathParser
  , expandSomePaths
  , module Path
  ) where

import Restyler.Prelude

import Data.Aeson (FromJSON, ToJSON)
import OptEnvConf (Parser, argument, help, metavar, reader, setting, str)
import Path
import Restyler.Monad.Directory

newtype SomePath = SomePath
  { unwrap :: FilePath
  }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, IsString, ToJSON)

somePathParser :: Parser SomePath
somePathParser =
  SomePath
    <$> setting
      [ help "Path to restyle (NOTE: paths outside the current directory are ignored)"
      , argument
      , reader str
      , metavar "PATH"
      ]

expandSomePaths :: MonadDirectory m => [SomePath] -> m [Path Rel File]
expandSomePaths = fmap concat . traverse (expandFilePath . (.unwrap))

expandFilePath :: MonadDirectory m => FilePath -> m [Path Rel File]
expandFilePath f =
  fromMaybe [] <$> runMaybeT (tryExpandDir f <|> fmap pure (validateRelFile f))

tryExpandDir :: MonadDirectory m => FilePath -> MaybeT m [Path Rel File]
tryExpandDir fp = do
  sb <- hoistMaybe $ parseSomeDir fp
  expandDir =<< fromSomeBase sb

expandDir :: MonadDirectory m => Path Rel Dir -> MaybeT m [Path Rel File]
expandDir d = do
  guardM $ lift $ doesDirectoryExist d
  fs <- map (d </>) . filter (not . isHidden) <$> lift (listDirectoryRecur d)
  mapMaybeMT ensureExistingNonLink fs

validateRelFile :: MonadDirectory m => FilePath -> MaybeT m (Path Rel File)
validateRelFile fp = do
  sb <- hoistMaybe $ parseSomeFile fp
  ensureExistingNonLink =<< fromSomeBase sb

ensureExistingNonLink
  :: MonadDirectory m => Path Rel File -> MaybeT m (Path Rel File)
ensureExistingNonLink f = do
  guardM $ lift $ doesFileExist f
  guardM $ lift $ not <$> pathIsSymbolicLink f
  pure f

fromSomeBase :: MonadDirectory m => SomeBase t -> MaybeT m (Path Rel t)
fromSomeBase sb = case sb of
  Abs p -> do
    -- Any absolute path not within the current directory is dropped here
    cwd <- lift getCurrentDirectory
    hoistMaybe $ stripProperPrefix cwd p
  Rel p -> pure p

isHidden :: Path Rel File -> Bool
isHidden = maybe False ((== '.') . head) . nonEmpty . toFilePath

mapMaybeMT :: Monad m => (a -> MaybeT m b) -> [a] -> MaybeT m [b]
mapMaybeMT f as = MaybeT $ Just <$> mapMaybeM (runMaybeT . f) as
