module Restyler.Prelude
    ( module X
    , module Restyler.Prelude
    ) where


--------------------------------------------------------------------------------
-- Safer re-exports (TODO: NoImplicitPrelude, Safe, etc)
--------------------------------------------------------------------------------
import Control.Monad as X
import Data.Foldable as X
import Data.Maybe as X
import Data.Proxy as X
import Data.Semigroup as X ((<>))
import Data.Text as X (Text)
import Data.Traversable as X

--------------------------------------------------------------------------------
-- Globally-useful utilities
--------------------------------------------------------------------------------
import qualified Data.Text as T

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = do
    result <- condition
    when result action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condition action = do
    result <- condition
    unless result action

tshow :: Show a => a -> Text
tshow = T.pack . show
