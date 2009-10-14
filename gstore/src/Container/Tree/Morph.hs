module Container.Tree.Morph where

import Generics.Morphisms
import qualified Container.Tree.Abstract as F

insert :: Ord k => Psi (k, v) (F.Tree k v)
insert (s@(k, v), t) =
  case t of
    F.Branch m w l r ->
      case k `compare` m of
        LT -> F.Branch m w (next (s, l))  (keep r)
        EQ -> F.Branch k v (keep l)       (keep r)
        GT -> F.Branch m w (keep l)       (next (s, r))
    F.Leaf -> F.Branch k v (make F.Leaf)  (make F.Leaf)

lookup :: Ord k => k -> Phi (F.Tree k v) (Maybe v)
lookup _ (F.Leaf          , _) = Nothing
lookup k (F.Branch c d l r, _) =
  case k `compare` c of
    LT -> l
    EQ -> Just d
    GT -> r

count :: Num n => Phi (F.Tree k v) n
count (F.Leaf          , _) = 0
count (F.Branch _ _ l r, _) = 1 + l + r

depth :: (Ord n, Num n) => Phi (F.Tree k v) n
depth (F.Leaf          , _) = 0
depth (F.Branch _ _ l r, _) = 1 + max l r

