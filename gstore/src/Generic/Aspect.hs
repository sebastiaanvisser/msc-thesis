{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , TypeFamilies
  , TypeOperators
  , ScopedTypeVariables
  #-}
module Generic.Aspect where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad
import Generic.Representation
import Generics.Regular.Base

class Monad m => Aspect a f m where
  produce :: f -> m (a f)
  query   :: a f -> m f
  modify  :: (f -> m (a f), a f -> m f)

-- Identity aspect, for demonstration only.

instance Monad m => Aspect I f m where
  produce = return . I
  query   = return . unI
  modify  = (return . I, return . unI)

-- Combine two different aspects into one.

instance (Monad m, Aspect a (b f) m, Aspect b f m) => Aspect (a :. b) f m where
  produce = produce >=> produce >=> return . O
  query   = (query >=> query) . unO
  modify  = ( fst modify >=> fst modify >=> return . O
            ,(snd modify >=> snd modify) . unO)

class Unwrap a where
  type UW a :: *
  unwrap :: a -> UW a

instance Unwrap (I f) where
  type UW (I f) = f
  unwrap = unI

instance Unwrap (a (b f)) => Unwrap ((a :. b) f) where
  type UW ((a :. b) f) = UW (a (b f))
  unwrap = unwrap . unO

