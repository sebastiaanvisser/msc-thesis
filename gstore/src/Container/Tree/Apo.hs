module Container.Tree.Apo where

import Annotation.Annotation
import Generics.Morphisms
import Generics.Representation
import qualified Container.Tree.Abstract as F

insertPsi :: Ord k => Psi (k, v) (F.Tree k v)
insertPsi (s@(k, v), t) =
  case t of
    F.Branch m w l r ->
      case k `compare` m of
        LT -> F.Branch m w (Right (s, Left l))   (Left (Left r))
        EQ -> F.Branch k v (Left (Left l))       (Left (Left r))
        GT -> F.Branch m w (Left (Left l))       (Right (s, Left r))
    F.Leaf -> F.Branch k v (Left (Right F.Leaf)) (Left (Right F.Leaf))

insert
  :: treeF ~ F.Tree k v
  => tree  ~ FixT1 a treeF
  => (Ord k, Annotation a treeF m)
  => (k, v) -> tree -> m tree
insert = apoT insertPsi

