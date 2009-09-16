module Generics.Annotation where

import Control.Monad
import Control.Arrow
import Control.Category
import Generics.Representation
import Prelude hiding ((.), id)

instance Monad m => Functor (Kleisli m a) where
  fmap f (Kleisli m) = Kleisli (liftM f . m)

type Produce a f m = Kleisli m (  f (FixT a f)) (a f (FixT a f))
type Query   a f m = Kleisli m (a f (FixT a f)) (  f (FixT a f))
type Modify  a f m = Kleisli m (  f (FixT a f)) (  f (FixT a f))
                  -> Kleisli m (a f (FixT a f)) (a f (FixT a f))

class Monad m => Annotation a f m where
  query   :: Query   a f m
  produce :: Produce a f m
  modify  :: Modify  a f m
  modify f = produce . f . query

instance Monad m => Annotation Id f m where
  query   = Kleisli (return . unId)
  produce = Kleisli (return . Id)

