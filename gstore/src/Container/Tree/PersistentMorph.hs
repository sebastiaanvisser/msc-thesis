module Container.Tree.PersistentMorph where

import Annotation.Persistent ()
import Data.Binary
import Generics.Morphisms
import Generics.Regular.Base ()
import Generics.Types
import Prelude hiding (sequence)
import Storage.FileHeap
import qualified Container.Tree.Abstract as F
import qualified Container.Tree.Morph    as M

type Tree k v = FixT1 Pointer (F.Tree k v)

insert :: (Ord k, Binary k, Binary v) => k -> v -> Tree k v -> HeapRW (Tree k v)
insert = curry (apoT M.insert)

count :: (Num n, Binary k, Binary v) => Tree k v -> HeapRO n
count = paraT M.count

depth :: (Ord n, Num n, Binary k, Binary v) => Tree k v -> HeapRO n
depth = paraT M.depth

lookup :: (Show v, Ord k, Binary k, Binary v) => k -> Tree k v -> HeapRO (Maybe v)
lookup k = paraT (M.lookup k)



