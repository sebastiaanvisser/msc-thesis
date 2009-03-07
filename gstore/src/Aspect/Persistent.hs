{-# LANGUAGE
    TypeOperators
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
 #-}
module Aspect.Persistent where

import Control.Applicative
import Control.Monad.State hiding (modify)
import Data.Binary
import Generic.Aspect
import Generic.Annotate
import Generic.Representation
import Prelude hiding (read)
import Storage.Storage

type PFix  f = Fix (f :. Pointer)
type PFixP f = Pointer (PFix f)

instance Binary f => Aspect Pointer f (Storage t) where
  produce = store
  query   = retrieve
  modify  = (store, \p -> query p <* delete p)

