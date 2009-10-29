module Container.Tree.Morph where

import Generics.Morphism.Apo
import Generics.Morphism.Para
import qualified Container.Tree.Abstract as F

insert :: Ord k => k -> v -> CoEndoA () (F.Tree k v)
insert k v ((), t) =
  case t of
    F.Branch m w l r ->
      case k `compare` m of
        LT -> F.Branch m w (next ((), r))  (keep r)
        EQ -> F.Branch k v (keep l)        (keep r)
        GT -> F.Branch m w (keep l)        (next ((), r))
    F.Leaf -> F.Branch k v (make F.Leaf)   (make F.Leaf)

lookup :: Ord k => k -> PsiA (F.Tree k v) (Maybe v)
lookup k = Psi $ \f ->
  case fst f of
    F.Leaf             -> Nothing
    F.Branch c d l r ->
      case k `compare` c of
        LT -> l
        EQ -> Just d
        GT -> r

size :: Num n => PsiA (F.Tree k v) n
size = Psi $ \f ->
  case fst f of
    F.Leaf             -> 0
    F.Branch _ _ l r -> 1 + l + r

depth :: (Ord n, Num n) => PsiA (F.Tree k v) n
depth = Psi $ \f ->
  case fst f of
    F.Leaf             -> 0
    F.Branch _ _ l r -> 1 + max l r

