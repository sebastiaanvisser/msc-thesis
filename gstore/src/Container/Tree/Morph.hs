module Container.Tree.Morph where

import Generics.Morphisms
import Generics.Representation
import qualified Container.Tree.Abstract as F

insert :: Ord k => Psi (k, v) (F.Tree k v)
insert (P s@(k, v) t) =
  case t of
    F.Branch m w l r ->
      case k `compare` m of
        LT -> F.Branch m w (next (P s l)) (keep r)
        EQ -> F.Branch k v (keep l)       (keep r)
        GT -> F.Branch m w (keep l)       (next (P s r))
    F.Leaf -> F.Branch k v (make F.Leaf)  (make F.Leaf)

lookup :: Ord k => k -> Phi (F.Tree k v) (Maybe v)
lookup _ (P F.Leaf _) = Nothing
lookup k (P (F.Branch c d l r) _) =
  case k `compare` c of
    LT -> l
    EQ -> Just d
    GT -> r

