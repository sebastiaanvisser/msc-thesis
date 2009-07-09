{-# LANGUAGE
    TypeOperators
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
 #-}
module Aspect.Persistent where

import Control.Applicative
import Data.Binary
import Generic.Aspect
import Generics.Regular.Base
import Generic.Representation
import Prelude hiding (read)
import Storage.FileStorage

type PFix  f = Fix (f :. Pointer)
type PFixP f = Pointer (PFix f)

instance Binary f => Aspect Pointer f (Storage t) where
  produce = store
  query   = retrieve
  modify  = (store, \p -> query p <* delete p)

instance Unwrap (Pointer a) where
  type UW (Pointer a) = Pointer a
  unwrap = id

