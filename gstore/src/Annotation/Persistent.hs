{-# LANGUAGE UndecidableInstances #-}
module Annotation.Persistent where

import Control.Applicative
import Control.Arrow
import Data.Binary
import Annotation.Annotation
import Generics.Types
import Storage.FileHeap

instance Binary (f (FixT Pointer f)) => AnnQ Pointer f HeapRO where
  query = Kleisli retrieve

instance Binary (f (FixT Pointer f)) => AnnQ Pointer f HeapRW where
  query = Kleisli (readAction . retrieve)

instance Binary (f (FixT Pointer f)) => AnnP Pointer f HeapRW where
  produce = Kleisli (error "TODO: `AnnP Pointer f HeapRW' not implemented yet")

instance Binary (f (FixT Pointer f)) => AnnM Pointer f HeapRW where

instance Binary (a f (FixT a f)) => Binary (FixT a f) where
  put = put . out
  get = In <$> get

