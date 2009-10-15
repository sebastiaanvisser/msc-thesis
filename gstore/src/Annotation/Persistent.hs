{-# LANGUAGE TypeSynonymInstances, UndecidableInstances #-}
module Annotation.Persistent where

import Control.Applicative
import Control.Arrow
import Control.Monad.Lazy
import Data.Binary
import Annotation.Annotation
import Generics.Types
import Heap.Heap

instance Binary (f (FixT Pointer f)) => AnnQ Pointer f HeapR where
  query = Kleisli retrieve

instance Binary (f (FixT Pointer f)) => AnnQ Pointer f HeapW where
  query = Kleisli (liftLazy . retrieve)

instance Binary (f (FixT Pointer f)) => AnnP Pointer f HeapW where
  produce = Kleisli store

instance Binary (f (FixT Pointer f)) => AnnM Pointer f HeapW where

instance Binary (a f (FixT a f)) => Binary (FixT a f) where
  put = put . out
  get = In <$> get

