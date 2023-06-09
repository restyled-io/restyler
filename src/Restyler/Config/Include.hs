module Restyler.Config.Include
  ( Include (..)
  , explicit
  , includePath
  ) where

import Restyler.Prelude

import Data.Aeson
import System.FilePath.Glob (Pattern, compile, decompile, match)

data Include
  = -- | @**\/*.hs@
    Include Pattern
  | -- | @!**\/*.temp@
    Negated Pattern
  deriving stock (Eq, Show)

instance FromJSON Include where
  parseJSON = withText "Include pattern" $ pure . fromString . unpack

instance ToJSON Include where
  toJSON (Include p) = toJSON $ pack $ decompile p
  toJSON (Negated p) = toJSON $ "!" <> pack (decompile p)

instance IsString Include where
  fromString ('!' : rest) = Negated $ compile rest
  fromString x = Include $ compile x

-- | Build an @'Include'@ matching a path exactly
explicit :: FilePath -> Include
explicit = Include . compile

-- | Determine if a set of @'Include'@s match a file
--
-- Don't try to over-think this. It works how you would expect, and you can
-- confirm in its test cases.
includePath :: [Include] -> FilePath -> Bool
includePath is fp = foldl' go False is
 where
  go :: Bool -> Include -> Bool
  go b (Include p) = b || p `match` fp
  go b (Negated p) = b && not (p `match` fp)
