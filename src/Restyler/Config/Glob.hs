{-# LANGUAGE AllowAmbiguousTypes #-}

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

newtype Glob a = Glob { unGlob :: String }
    deriving stock (Eq, Generic)
    deriving newtype Show

instance FromJSON (Glob a) where
    parseJSON = withText "Glob" $ pure . Glob . unpack

instance ToJSON (Glob a) where
    toJSON = String . pack . unGlob

class GlobTarget a where
    forMatch :: a -> String
    getCompOptions :: CompOptions

instance GlobTarget FilePath where
    forMatch = id
    getCompOptions = compDefault

instance GlobTarget Text where
    forMatch = unpack
    getCompOptions = compDefault
        { characterClasses = False
        , characterRanges = False
        , numberRanges = False
        }

instance GlobTarget (Name a) where
    forMatch = forMatch . toPathPart
    getCompOptions = compDefault
        { characterClasses = False
        , characterRanges = False
        , numberRanges = False
        }

match :: forall a . GlobTarget a => Glob a -> a -> Bool
match (Glob p) = Glob.match (compileWith (getCompOptions @a) p) . forMatch

matchAny :: (Foldable t, GlobTarget a) => [Glob a] -> t a -> Bool
matchAny globs = any $ \x -> any (`match` x) globs
