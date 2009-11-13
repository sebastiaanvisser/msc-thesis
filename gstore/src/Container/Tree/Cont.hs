{-# OPTIONS_GHC -F -pgmF she #-}
module Container.Tree.Cont where

import Control.Applicative
import Control.Monad
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

triplet :: (Applicative m, Monad m) => k -> v -> k -> v -> k -> v -> C.Produce a (Tree k v) m
triplet a0 b0 a1 b1 a2 b2 p =
  (| p (| (Branch a1 b1)
  (| p (| (Branch a0 b0) (p Leaf) (p Leaf) |) @ |)
  (| p (| (Branch a2 b2) (p Leaf) (p Leaf) |) @ |) |) @ |)

lookup :: (Monad m, Applicative m, Ord k) => k -> C.Query a (Tree k v) m (Maybe v)
lookup _ _ Leaf = (| Nothing |)
lookup k q (Branch c d l r) =
  case k `compare` c of
    EQ -> (| (Just d) |)
    LT -> q l
    GT -> q r

size :: (Num c, Applicative m) => C.Query a (Tree k b) m c
size _ Leaf =    (| 0 |)
size q t    = (| (| 1 |) + (| q (leftT t) + q (rightT t) |) |)

depth :: (Ord c, Num c, Applicative m, Monad m) => C.Query a (Tree k v) m c
depth _ Leaf =    (| 0 |)
depth q t    = (| (| 1 |) + (| max (q (leftT t)) (q (rightT t)) |) |)

alter :: (Monad m, Ord k) => (Maybe v -> Maybe v) -> k -> C.Modify a (Tree k v) m
alter f k _ p Leaf = maybe (p Leaf) (\v -> singleton k v p) (f Nothing)
alter _ k q p (Branch c d l r)
  | k > c     = q r >>= p . \s -> Branch c d l s
  | otherwise = q l >>= p . \s -> Branch c d s r

