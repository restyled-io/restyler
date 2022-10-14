module Restyler.Prelude
    ( module X
    , module Restyler.Prelude
    ) where

import Relude as X hiding (exitSuccess, readFile, readFileBS, writeFile)

import Blammo.Logging as X
import Control.Error.Util as X (hush, note)
import Control.Monad.Extra as X (eitherM, fromMaybeM, maybeM)
import Control.Monad.IO.Unlift as X (MonadUnliftIO(..))
import Data.Bitraversable as X (bimapM)
import Data.Char as X (isSpace)
import Data.Functor.Syntax as X ((<$$>))
import Data.Text as X (pack, unpack)
import Data.Traversable as X (for)
import GitHub.Data as X (Id, Name, URL(..), getUrl, mkId, mkName, untagName)
import Lens.Micro as X (Lens', lens, (.~), (^.), (^?))
import Lens.Micro.Mtl as X (view)
import System.Exit as X (ExitCode(..))
import UnliftIO.Async as X (race)
import UnliftIO.Concurrent as X (threadDelay)
import UnliftIO.Exception as X
    (Handler(..), IOException, finally, onException, throwIO, throwString)
import UnliftIO.Temporary as X (withSystemTempDirectory)

import Data.Aeson (Key)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (maximumBy, minimum, minimumBy)
import UnliftIO.Exception (handleAny)

maximumByMaybe :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumByMaybe f = \case
    [] -> Nothing
    xs -> Just $ maximumBy f xs

minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe = \case
    [] -> Nothing
    xs -> Just $ minimum xs

minimumByMaybe :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumByMaybe f = \case
    [] -> Nothing
    xs -> Just $ minimumBy f xs

-- | Ignore an exception, warning about it
warnIgnore :: Monoid a => (MonadUnliftIO m, MonadLogger m) => m a -> m a
warnIgnore = warnIgnoreWith mempty

-- | Ignore an exception, warning about it and returning the given result
warnIgnoreWith :: (MonadUnliftIO m, MonadLogger m) => a -> m a -> m a
warnIgnoreWith a = handleAny (\ex -> a <$ logWarn (msg ex))
  where
    msg :: Exception e => e -> Message
    msg ex =
        "Ignoring caught exception" :# ["exception" .= displayException ex]

-- | Inverse of @'any'@
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p = not . any p

insertIfMissing :: Key -> v -> KeyMap v -> KeyMap v
insertIfMissing k v m = KeyMap.unionWith const m $ KeyMap.singleton k v

exitCodeInt :: ExitCode -> Int
exitCodeInt = \case
    ExitSuccess -> 0
    ExitFailure x -> x
