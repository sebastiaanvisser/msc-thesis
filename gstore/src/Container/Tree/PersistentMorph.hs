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

type Map k v = FixT1 Pointer (F.Tree k v)

insert :: (Ord k, Binary k, Binary v) => k -> v -> Map k v -> HeapW (Map k v)
insert k v = coEndoMT (M.insert k v) ()

size :: (Num n, Binary k, Binary v, DSeq n) => Map k v -> HeapR n
size = paraMT' M.size

depth :: (Ord n, Num n, Binary k, Binary v, DSeq n) => Map k v -> HeapR n
depth = paraMT' M.depth

lookup :: (Show v, Ord k, Binary k, Binary v, DSeq v) => k -> Map k v -> HeapR (Maybe v)
lookup k = paraMT' (M.lookup k)

