module Restyler.Prelude
    ( module X
    , module Restyler.Prelude
    )
where

--------------------------------------------------------------------------------
-- Safe(r) re-exports
--------------------------------------------------------------------------------
import Prelude as X hiding
    (head, init, last, maximum, minimum, pred, read, readFile, succ, tail)

import Control.Arrow as X ((&&&), (***))
import Control.Error.Util as X (hush, note)
import Control.Exception.Safe as X
import Control.Monad as X
import Control.Monad.Except as X
import Control.Monad.Logger as X
import Control.Monad.Reader as X
import Data.Bifunctor as X
import Data.Char as X (isSpace)
import Data.Foldable as X hiding (maximumBy, minimumBy)
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
import GHC.Generics as X
import GHC.Stack as X
import GitHub.Data as X hiding (command)
import Safe as X

--------------------------------------------------------------------------------
-- Globally-useful utilities
--------------------------------------------------------------------------------
import qualified Data.Foldable as F
import qualified Data.Text as T

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

-- | @'Show'@ as @'Text'@
tshow :: Show a => a -> Text
tshow = T.pack . show

infixl 4 <$$>

-- | @'fmap'@ for doubly-wrapped values
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = fmap f <$> a

minimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy _ [] = Nothing
minimumBy f xs = Just $ F.minimumBy f xs

maximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy _ [] = Nothing
maximumBy f xs = Just $ F.maximumBy f xs

-- | Strip whitespace from the end of a @'Text'@
chomp :: Text -> Text
chomp = T.dropWhileEnd isSpace
