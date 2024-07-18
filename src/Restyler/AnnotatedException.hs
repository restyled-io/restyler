module Restyler.AnnotatedException
  ( Annotation (..)
  , checkpoint
  , AnnotatedException
  , withAnnotatedException
  , unannotatedException
  , findAnnotation
  ) where

import Restyler.Prelude

import Control.Exception.Annotated.UnliftIO
import Data.Annotation (tryAnnotations)
import Data.List.NonEmpty qualified as NE

withAnnotatedException
  :: (MonadUnliftIO m, Exception e) => m a -> (AnnotatedException e -> m b) -> m a
withAnnotatedException act with =
  act `catch` \aex -> do
    void $ with aex
    throwIO (exception aex)

unannotatedException :: AnnotatedException e -> e
unannotatedException = exception

findAnnotation :: forall a e. Typeable a => AnnotatedException e -> Maybe a
findAnnotation = fmap head . NE.nonEmpty . fst . tryAnnotations . annotations
