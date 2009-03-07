{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , TypeOperators
  #-}
module Aspect.Aspect where

import Control.Monad.State
import Generic.Annotate
import Generic.Representation

combine
  :: Monad m
  => (a -> m b)
  -> (b -> m (f (g c)))
  -> a -> m ((:.) f g c)
combine a b = a >=> liftM C . b

class Monad m => Aspect a f m where
  produce :: f -> m (a f)

-- Identity aspect, for demonstration only.

instance Monad m => Aspect Id f m where
  produce = return . Id

instance (Monad m, Aspect a (b f) m, Aspect b f m)
      => Aspect (a :. b) f m where
  produce = produce `combine` produce

