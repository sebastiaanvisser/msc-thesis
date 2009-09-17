module Container.Tree.Persistent where

import Data.Binary
import Storage.FileStorage
import Generics.Representation
import Generics.Regular.Base ()
import Generics.Cont
import Annotation.Persistent ()
import qualified Container.Tree.Abstract as F
import qualified Container.Tree.Cont     as C
-- import qualified Container.Tree.Apo      as M

type Tree k v = FixT Pointer (F.Tree k v)

empty :: (Binary k, Binary v) => Storage t (Tree k v)
empty = mkProducer C.empty

singleton :: (Binary k, Binary v) => k -> v -> Storage t (Tree k v)
singleton k v = mkProducer (C.singleton k v)

triplet :: (Binary k, Binary v) => k -> v -> k -> v -> k -> v -> Storage t (Tree k v)
triplet a0 b0 a1 b1 a2 b2 = mkProducer (C.triplet a0 b0 a1 b1 a2 b2)

lookup :: (Ord k, Binary k, Binary v) => k -> Tree k v -> Storage t (Maybe v)
lookup k = mkQuery (C.lookup k)

count :: (Num c, Binary k, Binary v) => Tree k v -> Storage t c
count = mkQuery C.count

depth :: (Ord c, Num c, Binary k, Binary v) => Tree k v -> Storage t c
depth = mkQuery C.depth

insert :: (Ord k, Binary k, Binary v) => k -> v -> Tree k v -> Storage t (Tree k v)
insert k v = mkModifier (C.insert k v)

