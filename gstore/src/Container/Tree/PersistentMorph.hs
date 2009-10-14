module Container.Tree.PersistentMorph where

import Annotation.Persistent ()
import Data.Binary
import Generics.Morphisms
import Generics.Regular.Base ()
import Generics.Representation
import Prelude hiding (sequence)
import Storage.FileHeap
import qualified Container.Tree.Abstract as F
import qualified Container.Tree.Morph    as M

type Tree k v = FixT1 Pointer (F.Tree k v)

lookup :: (Show v, Ord k, Binary k, Binary v) => k -> Tree k v -> HeapRO (Maybe v)
lookup k = paraT (M.lookup k)

insert :: (Ord k, Binary k, Binary v) => k -> v -> Tree k v -> HeapRW (Tree k v)
insert = curry (apoT M.insert)

