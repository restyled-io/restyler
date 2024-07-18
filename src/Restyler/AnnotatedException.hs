module Restyler.AnnotatedException
  ( findAnnotation

    -- * Re-exports
  , Annotation (..)
  , checkpoint
  , AnnotatedException (..)
  , throw
  , tryAnnotated
  , Handler (..)
  , catches
  , displayAnnotatedException
  ) where

import Restyler.Prelude

import Control.Exception.Annotated.UnliftIO
import Data.Annotation (tryAnnotations)
import Data.List.NonEmpty qualified as NE

findAnnotation :: forall a e. Typeable a => AnnotatedException e -> Maybe a
findAnnotation = fmap head . NE.nonEmpty . fst . tryAnnotations . annotations

displayAnnotatedException :: Exception e => AnnotatedException e -> Text
displayAnnotatedException aex@AnnotatedException {exception} =
  unlines
    [ "Annotated Exception"
    , ""
    , pack $ displayException exception
    , ""
    , maybe "" (pack . prettyCallStack) $ annotatedExceptionCallStack aex
    ]
