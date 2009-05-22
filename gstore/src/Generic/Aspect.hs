{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , TypeOperators
  #-}
module Generic.Aspect where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad
import Generic.Representation

class Monad m => Aspect a f m where
  produce :: f -> m (a f)
  query   :: a f -> m f
  modify  :: (f -> m (a f), a f -> m f)

-- Identity aspect, for demonstration only.

instance Monad m => Aspect Id f m where
  produce = return . Id
  query   = return . unId
  modify  = (return . Id, return . unId)

-- Combine two different aspects into one.

instance (Monad m, Aspect a (b f) m, Aspect b f m)
      => Aspect (a :. b) f m where
  produce = produce >=> produce >=> return . C
  query   = (query >=> query) . unC
  modify  = ( fst modify >=> fst modify >=> return . C
            ,(snd modify >=> snd modify) . unC)

