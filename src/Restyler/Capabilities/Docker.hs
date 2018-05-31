module Restyler.Capabilities.Docker
    ( MonadDocker(..)
    )
where

import Restyler.Prelude

class MonadDocker m where
    dockerRun :: [String] -> m ()
