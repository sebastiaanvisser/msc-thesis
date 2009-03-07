{-# LANGUAGE TypeOperators #-}
module Aspect.Test where

import Data.Binary
import Generic.Aspect
import Aspect.Debug
import Aspect.Persistent
import Generic.Annotate
import Generic.Representation
import Storage.Storage

type MyAspects = Pointer :. Debug :. Id
type MyFix  f = Fix (f :. MyAspects)
type MyFixM f = MyAspects (MyFix f)

myP
  :: (Show (f (MyFixM f)), Binary (f (MyFixM f)))
  => Producer f (MyFixM f) (Storage t)
  -> Storage t (MyFixM f)
myP p = p (produce . In . C)

