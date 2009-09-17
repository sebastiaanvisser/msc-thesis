{-# LANGUAGE UndecidableInstances #-}
module Annotation.Persistent where

import Control.Monad
import Control.Arrow
import Data.Binary
import Generics.Annotation
import Generics.Representation
import Storage.FileStorage

instance Binary (f (FixT Pointer f)) => Annotation Pointer f (Storage t) where
  query   = Kleisli retrieve
  produce = Kleisli store

instance Binary (a f (FixT a f)) => Binary (FixT a f) where
  put = put . out
  get = In `liftM` get
