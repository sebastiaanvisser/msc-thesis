{-# LANGUAGE
    TypeOperators
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
 #-}
module Aspect.Persistent where

import Control.Applicative
import Control.Monad.State
import Data.Binary
import Aspect.Aspect
import Generic.Annotate
import Generic.Representation
import Prelude hiding (read)
import Storage.Storage

type PFix  f = Fix (f :. Pointer)
type PFixP f = Pointer (PFix f)

instance Binary f => Aspect Pointer f (Storage t) where
  produce = store

-- Persistent producer.

persistentP
  :: Binary (f (PFixP f))
  => Producer f (PFixP f) (Storage t)
  -> Storage t (PFixP f)
persistentP p = p (produce . In . C)














-- Persistent query.

persistentQ
  :: (Binary (f (PFixP f)))
  => Query f (PFixP f) (Storage t) c
  -> PFixP f -> Storage t c
persistentQ q f = query f >>= worker
  where worker = q (\p -> query p >>= worker)

query
  :: Binary (f (PFixP f))
  => PFixP f
  -> Storage t (f (PFixP f))
query p = (unC . out) `liftM` retrieve p

-- Persistent modifier.

persistentM
  :: Binary (f (PFixP f))
  => Modifier f (PFixP f) (Storage t) c
  -> PFixP f -> Storage t c
persistentM m f = query f >>= worker
  where worker = m (store . In . C) (\p -> (query p <* delete p) >>= worker)

