{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Container.Tree.Persistent where

import Control.Monad
import Data.Binary
import Storage.FileStorage
import Generics.Representation
import Generics.Regular.Base ()
import Generics.Annotate
import Annotation.Persistent ()
import Annotation.Debug ()
import qualified Container.Tree.Abstract as F
import qualified Container.Tree.Apo      as P

{-

from the future:

test
  :: (Functor m, Annotation a (Tree k v) (FixT a (Tree k v)) m)
  => m (FixT a (Tree k v))
test = mkProducer empty

test2
  :: (Annotation a (Tree k v) (FixT a (Tree k v)) m, Functor m) =>
     k -> v -> m (FixT a (Tree k v))
test2 k v = mkProducer (singleton k v)

-}

type TreeAspects = {-I :. Debug :.-} Pointer

class (Show a, Binary a) => TreeClass a

type Tree  a b = AnnFix  (F.Tree a b) TreeAspects
type TreeP a b = AnnFixF (F.Tree a b) TreeAspects

triplet :: (TreeClass a, TreeClass b) => a -> b -> a -> b -> a -> b -> Storage t (TreeP a b)
triplet a0 b0 a1 b1 a2 b2 = mkProducer (F.triplet a0 b0 a1 b1 a2 b2)

singleton :: (TreeClass a, TreeClass b) => a -> b -> Storage t (TreeP a b)
singleton a b = mkProducer (F.singleton a b)

empty :: (TreeClass a, TreeClass b) => Storage t (TreeP a b)
empty = mkProducer F.empty

lookup :: (Ord a, Monad m, TreeClass a, TreeClass b) => a -> TreeP a b -> Storage t (m b)
lookup a = mkQuery (F.lookup a)

count :: (Num c, TreeClass a, TreeClass b) => TreeP a b -> Storage t c
count = mkQuery F.count

depth :: (Ord c, Num c, TreeClass a, TreeClass b) => TreeP a b -> Storage t c
depth = mkQuery F.depth

insert :: (Ord a, TreeClass a, TreeClass b) => a -> b -> TreeP a b -> Storage t (TreeP a b)
insert a b = mkModifier (F.insert a b)

