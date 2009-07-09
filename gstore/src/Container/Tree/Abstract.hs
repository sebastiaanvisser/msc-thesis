{-# LANGUAGE
    TypeFamilies
  , TypeOperators
  , TemplateHaskell
  , GADTs
  , KindSignatures
  , EmptyDataDecls
  , TypeSynonymInstances
  , MultiParamTypeClasses
  , RankNTypes
  #-}
module Container.Tree.Abstract where

import Data.Binary
import Generics.Annotate
import Generics.Regular
import Generics.Regular.Binary
import Generics.Regular.TH

-- Binary tree parametrized by key type, value type and recursive points.

data Tree a b f =
    Leaf
  | Branch {key :: a, val :: b, leftT :: f, rightT :: f}
  deriving (Eq, Ord, Show, Read)

-- Generic view.

$(deriveConstructors ''Tree)
$(deriveRegular ''Tree "PFTree")
type instance PF (Tree a b f) = PFTree a b f

-- Generically derived binary instance.

instance (Binary a, Binary b, Binary f) => Binary (Tree a b f) where
  put = gput
  get = gget

-- Basic functions.

empty :: Monad m => Producer (Tree a b) f m
empty p = p Leaf

singleton :: Monad m => a -> b -> Producer (Tree a b) f m
singleton a b p =
  do l0 <- p Leaf
     l1 <- p Leaf
     p (Branch a b l0 l1)

triplet :: Monad m => a -> b -> a -> b -> a -> b -> Producer (Tree a b) f m
triplet a0 b0 a1 b1 a2 b2 p =
  do l0 <- p Leaf
     l1 <- p Leaf
     l2 <- p Leaf
     l3 <- p Leaf
     lt <- p (Branch a0 b0 l0 l1)
     rt <- p (Branch a2 b2 l2 l3)
     p (Branch a1 b1 lt rt)

lookup :: (Monad m, Monad n, Ord a) => a -> Query (Tree a b) f m (n b)
lookup _ _ Leaf = return $ fail "element not found"
lookup a f (Branch c d l r) =
  case a `compare` c of
    EQ -> return (return d)
    LT -> f l
    GT -> f r

count :: (Num c, Monad m) => (f -> m c) -> Tree a b f -> m c
count _ Leaf = return 0
count f t =
  do a <- f (leftT  t)
     b <- f (rightT t)
     return (1 + a + b)

depth :: (Ord c, Num c, Monad m) => Query (Tree a b) f m c
depth _ Leaf = return 0
depth f t =
  do a <- f (leftT  t)
     b <- f (rightT t)
     return (1 + max a b)

insert :: (Monad m, Ord a) => a -> b -> Modifier (Tree a b) f m f
insert a b p _ Leaf = singleton a b p
insert a _ p f (Branch c d l r)
  | a > c     = f r >>= p .       Branch c d  l
  | otherwise = f l >>= p . flip (Branch c d) r

