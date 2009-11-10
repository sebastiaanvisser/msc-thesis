{-# LANGUAGE TypeSynonymInstances, UndecidableInstances #-}
module Annotation.Persistent where

import Control.Applicative
import Control.Arrow
import Control.Monad.Lazy
import Data.Binary
import Annotation.Annotation
import Generics.Types
import Heap.Heap

instance Binary (f (FixA Pointer f)) => AnnQ Pointer f HeapR where
  query = Kleisli (retrieve . out)

instance Binary (f (FixA Pointer f)) => AnnQ Pointer f HeapW where
  query = Kleisli (liftLazy . retrieve . out)

instance Binary (f (FixA Pointer f)) => AnnP Pointer f HeapW where
  produce = Kleisli (fmap In . store)

instance Binary (f (FixA Pointer f)) => AnnM Pointer f HeapW where

instance Binary (a f (FixA a f)) => Binary (FixA a f) where
  put = put . out
  get = In <$> get

