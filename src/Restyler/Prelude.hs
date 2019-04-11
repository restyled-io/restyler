module Restyler.Prelude
    ( module X
    , module Restyler.Prelude
    )
where

import RIO as X hiding (LogLevel(..), first, second)

import Control.Error.Util as X (hush, note)
import Control.Monad.Except as X
import Data.Bifunctor as X (first, second)
import GitHub.Data as X hiding (command)
import RIO.Char as X (isSpace)
import RIO.List as X (dropWhileEnd, find, headMaybe, minimumByMaybe)
import RIO.Text as X (encodeUtf8, pack, unpack)
import Safe as X (fromJustNote)

-- TODO: Move to RIO and its own logging facilities
import Control.Monad.Logger as X
    (LogLevel(..), MonadLogger, logDebugN, logErrorN, logInfoN, logWarnN)

--------------------------------------------------------------------------------
-- Globally-useful utilities
--------------------------------------------------------------------------------
import qualified RIO.Text as T

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8With T.lenientDecode

infixl 4 <$$>

-- | @'fmap'@ for doubly-wrapped values
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = fmap f <$> a

-- | Strip whitespace from the end of a @'Text'@
chomp :: Text -> Text
chomp = T.dropWhileEnd isSpace
