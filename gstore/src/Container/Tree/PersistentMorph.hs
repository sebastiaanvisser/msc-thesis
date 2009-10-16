{-# LANGUAGE OverlappingInstances #-}
module Container.Tree.PersistentMorph where

import Annotation.Persistent ()
import Data.Binary
import Generics.Morph
import Generics.Types
import Generics.Regular.Seq
import Heap.Heap
import Prelude hiding (sequence)
import qualified Container.Tree.Abstract as F
import qualified Container.Tree.Morph    as M

type Tree k v = FixT1 Pointer (F.Tree k v)

insert :: (Ord k, Binary k, Binary v) => k -> v -> Tree k v -> HeapW (Tree k v)
insert = curry (apoT M.insert)

count :: (Num n, Binary k, Binary v, DSeq n) => Tree k v -> HeapR n
count = paraT' M.count

depth :: (Ord n, Num n, Binary k, Binary v, DSeq n) => Tree k v -> HeapR n
depth = paraT' M.depth

lookup :: (Show v, Ord k, Binary k, Binary v, DSeq v) => k -> Tree k v -> HeapR (Maybe v)
lookup k = paraT' (M.lookup k)

