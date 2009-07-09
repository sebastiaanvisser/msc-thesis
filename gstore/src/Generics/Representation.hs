{-# LANGUAGE
    StandaloneDeriving
  , TypeFamilies
  , FlexibleContexts
  , UndecidableInstances
  , TypeOperators
  , GeneralizedNewtypeDeriving
  #-}
module Generics.Representation where

import Generics.Regular.Base
import Data.Binary

deriving instance Show f           => Show (I f)
deriving instance Show (f (Fix f)) => Show (Fix f)

instance Binary (f (Fix f)) => Binary (Fix f) where
  put = put . out
  get = In `fmap` get

-- Function composition at the type level.
-- Compose :: (* -> *) -> (* -> *) -> * -> *
infixr :.
newtype (f :. g) a = O { unO :: f (g a) }
  deriving Show

type AnnFix  f a =    Fix (f :. a)
type AnnFixF f a = a (Fix (f :. a))

