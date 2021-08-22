{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyler.Prelude
    ( module X
    , module Restyler.Prelude
    ) where

import RIO as X hiding (exitSuccess)

import Control.Error.Util as X (hush, note)
import Control.Monad.Extra as X (eitherM, fromMaybeM, maybeM)
import Data.Bitraversable as X (bimapM)
import Data.Functor.Syntax as X ((<$$>))
import GitHub.Data as X (Id, Name, URL(..), getUrl, mkId, mkName, untagName)
import RIO.Char as X (isSpace)
import RIO.List as X
    ( dropWhileEnd
    , find
    , genericLength
    , headMaybe
    , maximumByMaybe
    , maximumMaybe
    , minimumByMaybe
    , minimumMaybe
    )
import RIO.Text as X (pack, unpack)
import Safe as X (fromJustNote)

import qualified Data.HashMap.Lazy as HM
import qualified RIO.Text as T

-- Apparently they fixed a space leak in Data.List's version :shrug:
import qualified Data.Text.Internal.Functions as List (intersperse)

instance Display (Name a) where
    display = display . untagName

-- | Like @'withReader'@ for @'RIO'@
withRIO :: (env' -> env) -> RIO env a -> RIO env' a
withRIO f m = do
    env <- asks f
    runRIO env m

guardM :: (Monad m, Alternative m) => m Bool -> m ()
guardM mb = do
    b <- mb
    guard b

-- | Decode known-valid UTF-8
--
-- Uses @'lenientDecode'@:
--
-- Replace an invalid input byte with the Unicode replacement character U+FFFD.
--
decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8With T.lenientDecode

displayIntercalated :: Display a => Utf8Builder -> [a] -> Utf8Builder
displayIntercalated sep = mconcat . List.intersperse sep . map display

handles :: MonadUnliftIO m => [Handler m a] -> m a -> m a
handles = flip catches

handleTo
    :: (MonadUnliftIO m, Exception e1, Exception e2) => (e1 -> e2) -> m a -> m a
handleTo f = handle (throwIO . f)

tryTo :: (MonadUnliftIO m, Exception e) => (e -> b) -> m a -> m (Either b a)
tryTo f = fmap (first f) . try

-- | Inverse of @'any'@
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p = not . any p

insertIfMissing :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insertIfMissing = HM.insertWith $ \_ x -> x
