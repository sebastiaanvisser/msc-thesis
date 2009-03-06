{-# LANGUAGE ScopedTypeVariables #-}
module Generic.Fixpoints where

import Control.Applicative
import Generic.Core
import Prelude

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

class GFixpoints f where
  gfixpoints' :: f a -> Tree Int

instance GFixpoints Unit where
  gfixpoints' _ = Leaf 0

instance GFixpoints Id where
  gfixpoints' _ = Leaf 1

instance GFixpoints (K a) where
  gfixpoints' _ = Leaf 0

instance (GFixpoints f, GFixpoints g) => GFixpoints (Sum f g) where
  gfixpoints' _ = gfixpoints' (undefined :: f a)
           `Node` gfixpoints' (undefined :: g a)

instance (GFixpoints f, GFixpoints g) => GFixpoints (Prod f g) where
  gfixpoints' _ = (+) <$> gfixpoints' (undefined :: f a)
                      <*> gfixpoints' (undefined :: g a)

instance GFixpoints f => GFixpoints (Con f) where
  gfixpoints' _ = gfixpoints' (undefined :: f a)

