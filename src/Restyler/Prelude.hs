module Restyler.Prelude
    ( module X
    , module Restyler.Prelude
    )
where

-- Prefer Bifunctor first/second not Arrow
import RIO as X hiding (exitSuccess, first, second)

import Control.Error.Util as X (hush, note)
import Control.Monad.Extra as X (eitherM, maybeM)
import Data.Bifunctor as X (first, second)
import Data.Bitraversable as X (Bitraversable, bimapM)
import Data.Functor.Syntax as X ((<$$>))
import GitHub.Data as X (Id, Name, URL(..), getUrl, mkId, mkName, untagName)
import RIO.Char as X (isSpace)
import RIO.List as X
    (dropWhileEnd, find, headMaybe, minimumByMaybe, minimumMaybe)
import RIO.Text as X (encodeUtf8, pack, unpack)
import Safe as X (fromJustNote)

import qualified Data.Foldable as F
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as Set
import qualified RIO.Text as T

firstM :: (Bitraversable t, Applicative f) => (a -> f a') -> t a b -> f (t a' b)
firstM f = bimapM f pure

secondM
    :: (Bitraversable t, Applicative f) => (b -> f b') -> t a b -> f (t a b')
secondM = bimapM pure

-- | Like @'withReader'@ for @'RIO'@
withRIO :: (env' -> env) -> RIO env a -> RIO env' a
withRIO f m = do
    env <- asks f
    runRIO env m

-- | Decode known-valid UTF-8
--
-- Uses @'lenientDecode'@:
--
-- Replace an invalid input byte with the Unicode replacement character U+FFFD.
--
decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8With T.lenientDecode

handles :: MonadUnliftIO m => [Handler m a] -> m a -> m a
handles = flip catches

handleTo
    :: (MonadUnliftIO m, Exception e1, Exception e2) => (e1 -> e2) -> m a -> m a
handleTo f = handle (throwIO . f)

tryTo :: (MonadUnliftIO m, Exception e) => (e -> b) -> m a -> m (Either b a)
tryTo f = fmap (first f) . try

intersects :: (Foldable t1, Foldable t2, Ord a) => t1 a -> t2 a -> Bool
intersects a b = not $ Set.null $ Set.intersection a' b'
  where
    a' = Set.fromList $ F.toList a
    b' = Set.fromList $ F.toList b

-- | Inverse of @'any'@
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p = not . any p

insertIfMissing :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insertIfMissing = HM.insertWith $ flip const
