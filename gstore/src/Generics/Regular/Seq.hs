{-# LANGUAGE UndecidableInstances #-}
module Generics.Regular.Seq (DSeq (..), gdseq) where

import Data.List
import Generics.Regular.Base

class GSeq f where
  gseq :: (a -> b -> b) -> f a -> b -> b

instance GSeq I where
  gseq f (I x) = f x

instance DSeq a => GSeq (K a) where
  gseq _ (K x) = dseq x

instance GSeq U where
  gseq _ U = id

instance (GSeq f, GSeq g) => GSeq (f :+: g) where
  gseq f (L x) = gseq f x
  gseq f (R y) = gseq f y

instance (GSeq f, GSeq g) => GSeq (f :*: g) where
  gseq f (x :*: y) = gseq f x . gseq f y

instance GSeq f => GSeq (C c f) where
  gseq f (C x) = gseq f x

instance GSeq f => GSeq (S s f) where
  gseq f (S x) = gseq f x

-- | The deep and generic version of seq.

gdseq :: (Regular a, DSeq a, GSeq (PF a)) => a -> b -> b
gdseq p = gseq gdseq (from p)

-- 

class DSeq a where
  dseq   :: a -> b -> b
  dseqId :: a -> a
  dseqId a = dseq a a

instance DSeq Int  where dseq = seq
instance DSeq Char where dseq = seq

instance DSeq a => DSeq [a] where
  dseq xs b = foldl' (flip dseq) b xs

instance DSeq a => DSeq (Maybe a) where
  dseq Nothing  b = b
  dseq (Just a) b = dseq a b

-- instance (Regular a, GSeq (PF a)) => DSeq a where
--   dseq = gdseq

