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
import Generics.Aspect
-- import Generics.Regular.Base
-- import Generics.Representation
import Prelude hiding (read)
import Storage.FileStorage


data Ptr f a = Ptr { unPtr :: Pointer (f a) }

-- type PFix  f = AnnFix Ptr
-- type PFixP f = Pointer (PFix f)

instance Binary (f c) => Aspect Ptr f c (Storage t) where
  produce a = Ptr <$> store a
  query     = retrieve . unPtr
--   modify    = (produce, \p -> query p <* delete p)

-- instance Unwrap (Pointer a) where
--   type UW (Pointer a) = Pointer a
--   unwrap = id

