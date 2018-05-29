module Restyler.Prelude.NoApp
    ( module X
    , module Restyler.Prelude.NoApp
    )
where

--------------------------------------------------------------------------------
-- Safe(r) re-exports
--------------------------------------------------------------------------------
import Prelude as X hiding
    (head, init, last, maximum, minimum, pred, read, succ, tail)

import Control.Error.Util as X (hush, note)
import Control.Exception.Safe as X
import Control.Monad as X
import Control.Monad.Except as X
import Control.Monad.Logger as X
import Control.Monad.Reader as X
import Data.Bifunctor as X
import Data.Char as X (isSpace)
import Data.Foldable as X
import Data.List as X
    (dropWhileEnd, find, foldl', isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe as X hiding (fromJust)
import Data.Proxy as X
import Data.Semigroup as X ((<>))
import Data.Text as X (Text, pack, strip, unpack)
import Data.Text.Encoding as X
import Data.Traversable as X
import Data.Vector as X (Vector)
import Data.Void as X
import Safe as X

--------------------------------------------------------------------------------
-- Globally-useful utilities
--------------------------------------------------------------------------------
import qualified Data.Text as T

-- | @'any'@ lifted to @'Monad'@
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM p (x : xs) = p x >>= \r -> if r then pure True else anyM p xs

-- | @'when'@ with a monadic condition
--
-- > x <- someMonadicConditional
-- > when x $ do
-- >     someMonadicAction
-- >
-- > whenM someMonadicConditional someMonadicAction
--
whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = do
    result <- condition
    when result action

-- | Same for @'unless'@
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condition action = do
    result <- condition
    unless result action

-- | Like @'hush'@, but for @'MonadError' e m@ instead of @'Either'@
hushM :: MonadError e m => m a -> m (Maybe a)
hushM f = fmap Just f `catchError` const (pure Nothing)

-- | @'Show'@ as @'Text'@
tshow :: Show a => a -> Text
tshow = T.pack . show

infixl 4 <$$>

-- | @'fmap'@ for doubly-wrapped values
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = fmap f <$> a
