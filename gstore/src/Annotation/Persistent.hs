{-# LANGUAGE UndecidableInstances #-}
module Annotation.Persistent where

import Control.Monad
import Control.Arrow
import Data.Binary
import Annotation.Annotation
import Generics.Representation
import Storage.FileHeap

-- instance Binary (f (FixT Pointer f)) => Annotation Pointer f (Storage t) where
--   query   = Kleisli retrieve
--   produce = Kleisli store

instance Binary (f (FixT Pointer f)) => AnnQ Pointer f HeapRO where
  query = Kleisli retrieve

instance Binary (f (FixT Pointer f)) => AnnQ Pointer f HeapRW where query   = Kleisli (\a -> lazy (retrieve a))
instance Binary (f (FixT Pointer f)) => AnnP Pointer f HeapRW where produce = Kleisli undefined
instance Binary (f (FixT Pointer f)) => AnnM Pointer f HeapRW where

instance Binary (a f (FixT a f)) => Binary (FixT a f) where
  put = put . out
  get = In `liftM` get
