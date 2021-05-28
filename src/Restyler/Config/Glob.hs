-- | Small wrapper over @'System.FilePath.Glob.Pattern'@
module Restyler.Config.Glob
    ( Glob(..)
    , match
    , matchText
    )
where

import Restyler.Prelude

import Data.Aeson
import System.FilePath.Glob hiding (match)
import qualified System.FilePath.Glob as Glob

newtype Glob = Glob { unGlob :: Pattern }
    deriving stock (Eq, Generic)
    deriving newtype Show

instance FromJSON Glob where
    parseJSON = withText "Glob" $ pure . Glob . compile . unpack

instance ToJSON Glob where
    toJSON = String . pack . decompile . unGlob

match :: Glob -> FilePath -> Bool
match (Glob p) = Glob.match p

matchText :: Glob -> Text -> Bool
matchText g = match g . unpack
