module Container.Tree.PersistentCont where

import Annotation.Persistent ()
import Control.Applicative
import Data.Binary
import Data.Maybe
import Generics.Cont
import Generics.Regular.Base ()
import Generics.Types
import Heap.Heap hiding (size)
import Prelude hiding (lookup, null)
import qualified Container.Tree.Abstract as F
import qualified Container.Tree.Cont     as C

type Map k v = FixA2 Pointer (F.Tree k v)

empty :: (Binary k, Binary v) => HeapW (Map k v)
empty = mkProducer C.empty

singleton :: (Binary k, Binary v) => k -> v -> HeapW (Map k v)
singleton k v = mkProducer (C.singleton k v)

triplet :: (Binary k, Binary v) => k -> v -> k -> v -> k -> v -> HeapW (Map k v)
triplet a0 b0 a1 b1 a2 b2 = mkProducer (C.triplet a0 b0 a1 b1 a2 b2)

lookup :: (Ord k, Binary k, Binary v) => k -> Map k v -> HeapR (Maybe v)
lookup k = mkQuery (C.lookup k)

(!) :: (Ord k, Binary k, Binary a) => Map k a -> k -> HeapR a
(!) t k = fromMaybe (error "element not in the map") <$> lookup k t

member :: (Ord k, Binary k, Binary a) => k -> Map k a -> HeapR Bool
member k t = isJust <$> lookup k t

notMember :: (Ord k, Binary k, Binary a) => k -> Map k a -> HeapR Bool
notMember k t = not . isJust <$> lookup k t

size :: (Num c, Binary k, Binary v) => Map k v -> HeapR c
size = mkQuery C.size

null :: (Binary k, Binary v) => Map k v -> HeapR Bool
null t = (== (0 :: Integer)) <$> size t

depth :: (Ord c, Num c, Binary k, Binary v) => Map k v -> HeapR c
depth = mkQuery C.depth

alter :: (Ord k, Binary k, Binary v) => (Maybe v -> Maybe v) -> k -> Map k v -> HeapW (Map k v)
alter f k = mkModifier (C.alter f k)

insert :: (Ord k, Binary k, Binary v) => k -> v -> Map k v -> HeapW (Map k v)
insert k v = mkModifier (C.alter (const (Just v)) k)

delete :: (Ord k, Binary k, Binary v) => k -> Map k v -> HeapW (Map k v)
delete k = mkModifier (C.alter (const Nothing) k)

adjust :: (Ord k, Binary k, Binary v) => (v -> v) -> k -> Map k v -> HeapW (Map k v)
adjust f k = mkModifier (C.alter (fmap f) k)

