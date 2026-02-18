-- |
--
-- Module      : Restyler.Prelude
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Prelude
  ( module X
  , module Restyler.Prelude
  ) where

import Blammo.Logging as X
import Blammo.Logging.Logger (flushLogger)
import Blammo.Logging.Logger as X (HasLogger (..), withLogger)
import Blammo.Logging.WithLogger as X (WithLogger (..))
import Control.Error.Util as X (hush, note)
import Control.Monad.Catch as X (MonadMask)
import Control.Monad.Extra as X (eitherM, fromMaybeM, maybeM)
import Control.Monad.IO.Unlift as X (MonadUnliftIO (..))
import Data.Bitraversable as X (bimapM)
import Data.Char as X (isSpace)
import Data.Functor.Syntax as X ((<$$>))
import Data.List (minimum)
import Data.Text as X (pack, unpack)
import Data.Traversable as X (for)
import Data.Vector as X (Vector)
import Lens.Micro as X (Lens', lens, to, (.~), (^.), (^?))
import Lens.Micro.Mtl as X (view)
import Path as X (Abs, Dir, File, Path, Rel, toFilePath)
import Relude as X hiding (All (..), readFile, readFileBS, reader, writeFile)
import Restyler.OrphanInstances ()
import System.Exit as X (ExitCode (..))
import UnliftIO.Async as X (race)
import UnliftIO.Concurrent as X (threadDelay)
import UnliftIO.Exception as X (finally)
import UnliftIO.Temporary as X (withSystemTempDirectory)

logTrace :: (HasCallStack, MonadLogger m) => Message -> m ()
logTrace = logOther $ LevelOther "trace"

logProc
  :: ( HasCallStack
     , HasLogger env
     , MonadIO m
     , MonadLogger m
     , MonadReader env m
     )
  => String
  -> [String]
  -> m ()
logProc cmd args = do
  logDebug $ unwords (map pack $ "exec" : cmd : args) :# []
  flushLogger

readNat :: String -> Either String Natural
readNat n = first (const $ "Not a valid natural number: " <> n) (readEither n)

minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe = \case
  [] -> Nothing
  xs -> Just $ minimum xs

-- | Inverse of @'any'@
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p = not . any p
