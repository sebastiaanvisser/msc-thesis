module Container.Tree.Apo where

import Control.Applicative
import Generics.Aspect
import Generics.Morphisms
import Generics.Representation
import qualified Container.Tree.Abstract as F

insert :: Ord a => Psi (a, b) (F.Tree a b)
insert ((a, b), t) =
  case t of
    F.Branch c d l r ->
      case a `compare` c of
        LT -> F.Branch c d (Right ((a, b), Left l)) (Left (Left r))
        EQ -> F.Branch a b (Left (Left l))          (Left (Left r))
        GT -> F.Branch c d (Left (Left l))          (Right ((a, b), Left r))
    F.Leaf -> F.Branch a b (Left (Right F.Leaf))    (Left (Right F.Leaf))

insertA
  :: tree ~ AnnFixF g (F.Tree a b)
  => (Ord a, Applicative m, Aspect g (F.Tree a b) (AnnFix g (F.Tree a b)) m)
  => (a, b) -> tree -> m tree
insertA = apoA insert

insertId
  :: tree ~ MuF (F.Tree a b)
  => Ord a => (a, b) -> tree -> IO tree
insertId = apoA insert

{-leaf :: Fix (F.Tree a b)
leaf = In F.Leaf

branch :: a -> b -> Fix (F.Tree a b) -> Fix (F.Tree a b) -> Fix (F.Tree a b)
branch k v l r = In (F.Branch k v l r)

tree0 = branch 10 20 leaf leaf
tree1 = branch 5  4  leaf leaf
tree2 = branch 99 88 tree0 tree1
tree4 = insert 3 33 tree2
-}

