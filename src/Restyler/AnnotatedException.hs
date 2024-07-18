module Restyler.AnnotatedException
  ( Annotation (..)
  , checkpoint
  , AnnotatedException
  , tryAnnotated
  , unannotatedException
  , findAnnotation
  ) where

import Restyler.Prelude

import Control.Exception.Annotated.UnliftIO
import Data.Annotation (tryAnnotations)
import Data.List.NonEmpty qualified as NE

unannotatedException :: AnnotatedException e -> e
unannotatedException = exception

findAnnotation :: forall a e. Typeable a => AnnotatedException e -> Maybe a
findAnnotation = fmap head . NE.nonEmpty . fst . tryAnnotations . annotations
