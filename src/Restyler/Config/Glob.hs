-- | Small wrapper over @'System.FilePath.Glob.Pattern'@
module Restyler.Config.Glob
    ( Glob
    , glob
    , match
    )
where

import Restyler.Prelude

import Data.Aeson
import System.FilePath.Glob hiding (glob, match)
import qualified System.FilePath.Glob as Glob

newtype Glob = Glob { unGlob :: Pattern }
    deriving stock (Eq, Generic)
    deriving newtype Show

glob :: String -> Glob
glob = Glob . compile

instance FromJSON Glob where
    parseJSON = withText "Glob" $ pure . glob . unpack

instance ToJSON Glob where
    toJSON = String . pack . decompile . unGlob

match :: Glob -> String -> Bool
match (Glob p) = Glob.match p
