module Container.Tree.Apo where

import Control.Applicative
import Generics.Annotation
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
  :: tree ~ FixT1 g (F.Tree a b)
  => (Ord a, Applicative m, Annotation g (F.Tree a b) (FixT g (F.Tree a b)) m)
  => (a, b) -> tree -> m tree
insertA = apoT insert

