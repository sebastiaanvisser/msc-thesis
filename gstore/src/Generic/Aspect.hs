{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , TypeOperators
  #-}
module Generic.Aspect where

import Control.Monad
import Generic.Representation

class Monad m => Aspect a f m where
  produce :: f -> m (a f)
  query   :: a f -> m f
  modify  :: (f -> m (a f), a f -> m f)
  modify = (produce, query)

-- Identity aspect, for demonstration only.

instance Monad m => Aspect Id f m where
  produce = return . Id
  query   = return . unId

-- Combine two different aspects into one.

instance (Monad m, Aspect a (b f) m, Aspect b f m)
      => Aspect (a :. b) f m where
  produce = produce >=> liftM C . produce
  query   = (query   >=> query) . unC

