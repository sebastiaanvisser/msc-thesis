module Generics.Annotation where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Generics.Representation

type Produce a f c m = Kleisli m (  f c) (a f c)
type Query   a f c m = Kleisli m (a f c) (  f c)
type Modify  a f c m = Kleisli m (  f c) (  f c)
                    -> Kleisli m (a f c) (a f c)

class Monad m => Annotation a f c m where
  query   :: Query   a f c m
  produce :: Produce a f c m
  modify  :: Modify  a f c m
  modify f = produce . f . query

instance Monad m => Annotation Id f c m where
  query   = Kleisli (return . unId)
  produce = Kleisli (return . Id)

