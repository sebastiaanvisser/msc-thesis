module Generics.Regular.Fixpoints where

import Control.Applicative
import Generics.Regular.Base
import Prelude

-- Tree structure to store fixed points as found in the data type.

data Tree a = Leaf a | Node (Tree a) (Tree a)
 deriving Show

instance Applicative Tree where
  pure = Leaf
  Leaf f   <*> Leaf x   = Leaf (f x)
  Node f g <*> Node x y = Node (f <*> x) (g <*> y)
  _        <*> _        = error "non-isomorphic applicative functor usage"

foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree l _ (Leaf x)   = l x
foldTree l n (Node x y) = (foldTree l n x) `n` (foldTree l n y)

sum :: Tree Int -> Int
sum = foldTree id (+)

instance Functor Tree where
  fmap f = foldTree (Leaf . f) Node

-- Generic functions to compute fixed points.

class GFixp f where
  fixp :: f a -> Tree Int

instance GFixp I where
  fixp _ = Leaf 1

instance GFixp U where
  fixp _ = Leaf 0

instance GFixp (K a) where
  fixp _ = Leaf 0

instance (GFixp f, GFixp g) => GFixp (f :+: g) where
  fixp _ = fixp (undefined :: f a)
    `Node` fixp (undefined :: g a)

instance (GFixp f, GFixp g) => GFixp (f :*: g) where
  fixp _ = (+) <$> fixp (undefined :: f a)
               <*> fixp (undefined :: g a)

instance GFixp f => GFixp (C c f) where
  fixp _ = fixp (undefined :: f a)

instance GFixp f => GFixp (S s f) where
  fixp _ = fixp (undefined :: f a)

