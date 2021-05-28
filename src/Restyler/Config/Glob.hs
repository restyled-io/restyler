-- | Small wrapper over @'System.FilePath.Glob.Pattern'@
module Restyler.Config.Glob
    ( Glob(..)
    , GlobTarget(..)
    , match
    , matchAny
    )
where

import Restyler.Prelude

import Data.Aeson
import GitHub.Data (toPathPart)
import System.FilePath.Glob hiding (match)
import qualified System.FilePath.Glob as Glob

newtype Glob a = Glob { unGlob :: Pattern }
    deriving stock (Eq, Generic)
    deriving newtype Show

instance FromJSON (Glob a) where
    parseJSON = withText "Glob" $ pure . Glob . compile . unpack

instance ToJSON (Glob a) where
    toJSON = String . pack . decompile . unGlob

class GlobTarget a where
    forMatch :: a -> String

instance GlobTarget FilePath where
    forMatch = id

instance GlobTarget Text where
    forMatch = unpack

instance GlobTarget (Name a) where
    forMatch = forMatch . toPathPart

match :: GlobTarget a => Glob a -> a -> Bool
match (Glob p) = Glob.match p . forMatch

matchAny :: (Foldable t, GlobTarget a) => [Glob a] -> t a -> Bool
matchAny globs = any $ \x -> any (`match` x) globs
