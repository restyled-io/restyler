module Restyler.Prelude
    ( module X
    , module Restyler.Prelude
    ) where

import RIO as X
    hiding
        ( LogLevel(..)
        , LogSource
        , exitSuccess
        , logDebug
        , logError
        , logInfo
        , logOther
        , logWarn
        )

import Blammo.Logging as X
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

import Data.Aeson (Key)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified RIO.Text as T

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

insertIfMissing :: Key -> v -> KeyMap v -> KeyMap v
insertIfMissing k v m = KeyMap.unionWith const m $ KeyMap.singleton k v

exitCodeInt :: ExitCode -> Int
exitCodeInt = \case
    ExitSuccess -> 0
    ExitFailure x -> x
