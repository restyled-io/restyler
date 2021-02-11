{-# OPTIONS_GHC -Wno-orphans #-}

module Restyler.Prelude
    ( module X
    , module Restyler.Prelude
    )
where

import RIO as X hiding
    ( logDebug
    , logError
    , logInfo
    , logOther
    , logSticky
    , logStickyDone
    , logWarn
    , readFileBinary
    , readFileUtf8
    , writeFileUtf8
    )

import Control.Error.Util as X (hush, note)
import Control.Monad.Except as X (ExceptT(..), MonadError(..), runExceptT)
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

import qualified Data.Foldable as F
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as Set
import qualified RIO.Text as T

-- Apparently they fixed a space leak in Data.List's version :shrug:
import qualified Data.Text.Internal.Functions as List (intersperse)

instance (MonadUnliftIO m, Exception e) => MonadUnliftIO (ExceptT e m) where
    withRunInIO exceptToIO = ExceptT $ try $ do
        withRunInIO $ \runInIO ->
            exceptToIO (runInIO . (either throwIO pure <=< runExceptT))

onError :: MonadError e m => m a -> (e -> m ()) -> m a
onError f g = f `catchError` \ex -> do
    g ex
    throwError ex

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
