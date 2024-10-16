{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
--
-- Module      : Restyler.Option
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Option
  ( Option (..)
  , HasOption (..)
  , lookupOption
  , lookupOptionDefault

    -- * Construction
  , OptionSpec (..)
  , envOption
  , optOption
  ) where

import Restyler.Prelude

import Data.Semigroup qualified as Semigroup
import Env qualified
import Options.Applicative qualified as Opt

newtype Option t a = Option
  { unwrap :: Semigroup.Last a
  }
  deriving newtype (Semigroup, FromJSON)

class HasOption t env a where
  getOption :: env -> Maybe (Option t a)

lookupOption
  :: forall t env m a. (MonadReader env m, HasOption t env a) => m (Maybe a)
lookupOption = do
  mOption <- asks $ getOption @t
  pure $ Semigroup.getLast . (.unwrap) <$> mOption

lookupOptionDefault
  :: forall t env m a. (MonadReader env m, HasOption t env a) => a -> m a
lookupOptionDefault def = fromMaybe def <$> lookupOption @t

data OptionSpec t a = OptionSpec
  { envParser :: Env.Parser Env.Error (Maybe a)
  , optParser :: Opt.Parser (Maybe a)
  }

envOption :: OptionSpec t a -> Env.Parser Env.Error (Maybe (Option t a))
envOption spec = fmap (Option . Semigroup.Last) <$> spec.envParser

optOption :: OptionSpec t a -> Opt.Parser (Maybe (Option t a))
optOption spec = fmap (Option . Semigroup.Last) <$> spec.optParser
