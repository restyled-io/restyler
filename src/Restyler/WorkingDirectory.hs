module Restyler.WorkingDirectory
    ( HasWorkingDirectory(..)
    ) where

import Restyler.Prelude

class HasWorkingDirectory env where
    workingDirectoryL :: Lens' env FilePath
