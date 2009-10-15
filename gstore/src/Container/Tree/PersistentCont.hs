module Container.Tree.PersistentCont where

import Annotation.Persistent ()
import Data.Binary
import Generics.Cont
import Generics.Regular.Base ()
import Generics.Types
import qualified Container.Tree.Abstract as F
import qualified Container.Tree.Cont     as C
import Heap.Heap

type Tree k v = FixT1 Pointer (F.Tree k v)

empty :: (Binary k, Binary v) => HeapW (Tree k v)
empty = mkProducer C.empty

singleton :: (Binary k, Binary v) => k -> v -> HeapW (Tree k v)
singleton k v = mkProducer (C.singleton k v)

triplet :: (Binary k, Binary v) => k -> v -> k -> v -> k -> v -> HeapW (Tree k v)
triplet a0 b0 a1 b1 a2 b2 = mkProducer (C.triplet a0 b0 a1 b1 a2 b2)

lookup :: (Ord k, Binary k, Binary v) => k -> Tree k v -> HeapR (Maybe v)
lookup k = mkQuery (C.lookup k)

count :: (Num c, Binary k, Binary v) => Tree k v -> HeapR c
count = mkQuery C.count

depth :: (Ord c, Num c, Binary k, Binary v) => Tree k v -> HeapR c
depth = mkQuery C.depth

insert :: (Ord k, Binary k, Binary v) => k -> v -> Tree k v -> HeapW (Tree k v)
insert k v = mkModifier (C.insert k v)

