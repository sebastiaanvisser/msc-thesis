{-# LANGUAGE
    TypeOperators
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
 #-}
module Aspect.Persistent where

import Control.Applicative
import Data.Binary
import Generic.Aspect
import Generic.Representation
import Prelude hiding (read)
import Storage.FileStorage

type PFix  f = Fix (f :. Pointer)
type PFixP f = Pointer (PFix f)

instance Binary f => Aspect Pointer f (Storage t) where
  produce = store
  query   = retrieve
  modify  = (store, \p -> query p <* delete p)

