module Restyler.Prelude
  ( module X
  , module Restyler.Prelude
  ) where

import Relude as X hiding (All (..), readFile, readFileBS, writeFile)

import Blammo.Logging as X
import Control.Error.Util as X (hush, note)
import Control.Monad.Extra as X (eitherM, fromMaybeM, maybeM)
import Control.Monad.IO.Unlift as X (MonadUnliftIO (..))
import Data.Aeson as X (FromJSON, ToJSON)
import Data.Bitraversable as X (bimapM)
import Data.Char as X (isSpace)
import Data.Functor.Syntax as X ((<$$>))
import Data.Text as X (pack, unpack)
import Data.Traversable as X (for)
import Data.Vector as X (Vector)
import Lens.Micro as X (Lens', lens, to, (.~), (^.), (^?))
import Lens.Micro.Mtl as X (view)
import System.Exit as X (ExitCode (..))
import UnliftIO.Async as X (race)
import UnliftIO.Concurrent as X (threadDelay)
import UnliftIO.Exception as X (finally)
import UnliftIO.Temporary as X (withSystemTempDirectory)

import Blammo.Logging.Logger (flushLogger)
import Data.Aeson (Key)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (minimum, minimumBy, (!!))

logTrace :: (MonadLogger m, HasCallStack) => Message -> m ()
logTrace = logOther $ LevelOther "trace"

logProc
  :: ( MonadIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasCallStack
     )
  => String
  -> [String]
  -> m ()
logProc cmd args = do
  logDebug $ unwords (map pack $ "exec" : cmd : args) :# []
  flushLogger

minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe = \case
  [] -> Nothing
  xs -> Just $ minimum xs

minimumByMaybe :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumByMaybe f = \case
  [] -> Nothing
  xs -> Just $ minimumBy f xs

-- | Safe version of '(!!)'
(!?) :: [a] -> Int -> Maybe a
xs !? i
  | length xs > i = Just $ xs !! i
  | otherwise = Nothing

infixl 9 !?

-- | Inverse of @'any'@
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p = not . any p

insertIfMissing :: Key -> v -> KeyMap v -> KeyMap v
insertIfMissing k v m = KeyMap.unionWith const m $ KeyMap.singleton k v
