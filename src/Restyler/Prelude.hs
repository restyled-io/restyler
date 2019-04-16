module Restyler.Prelude
    ( module X
    , module Restyler.Prelude
    )
where

-- Prefer Bifunctor first/second not Arrow
import RIO as X hiding (first, second)

import Control.Error.Util as X (hush, note)
import Data.Bifunctor as X (first, second)
import GitHub.Data as X (Id, Name, URL(..), getUrl, mkId, mkName, untagName)
import RIO.Char as X (isSpace)
import RIO.List as X (dropWhileEnd, find, headMaybe, minimumByMaybe)
import RIO.Text as X (encodeUtf8, pack, unpack)
import Safe as X (fromJustNote)

import qualified RIO.Text as T

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

infixl 4 <$$>

-- | @'fmap'@ for doubly-wrapped values
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = fmap f <$> a

handles :: MonadUnliftIO m => [Handler m a] -> m a -> m a
handles = flip catches
