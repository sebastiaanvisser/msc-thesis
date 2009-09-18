module Container.Tree.Cont where

import Container.Tree.Abstract
import Prelude hiding (lookup)
import qualified Generics.Cont as C

empty :: C.Produce a (Tree k v) m
empty p = p Leaf

singleton :: Monad m => k -> v -> C.Produce a (Tree k v) m
singleton k v p =
  do l0 <- p Leaf
     l1 <- p Leaf
     p (Branch k v l0 l1)

triplet :: Monad m => k -> v -> k -> v -> k -> v -> C.Produce a (Tree k v) m
triplet a0 b0 a1 b1 a2 b2 p =
  do l0 <- p Leaf
     l1 <- p Leaf
     l2 <- p Leaf
     l3 <- p Leaf
     lt <- p (Branch a0 b0 l0 l1)
     rt <- p (Branch a2 b2 l2 l3)
     p (Branch a1 b1 lt rt)

lookup :: (Monad m, Ord k) => k -> C.Query a (Tree k v) m (Maybe v)
lookup _ _ Leaf = return Nothing
lookup k q (Branch c d l r) =
  case k `compare` c of
    EQ -> return (Just d)
    LT -> q l
    GT -> q r

count :: (Num c, Monad m) => C.Query a (Tree k b) m c
count _ Leaf = return 0
count q t =
  do k <- q (leftT  t)
     v <- q (rightT t)
     return (1 + k + v)

depth :: (Ord c, Num c, Monad m) => C.Query a (Tree k v) m c
depth _ Leaf = return 0
depth q t =
  do k <- q (leftT  t)
     v <- q (rightT t)
     return (1 + max k v)

insert :: (Monad m, Ord k) => k -> v -> C.Modify a (Tree k v) m
insert k v _ p Leaf = singleton k v p
insert k _ q p (Branch c d l r)
  | k > c     = q r >>= p .       Branch c d  l
  | otherwise = q l >>= p . flip (Branch c d) r

