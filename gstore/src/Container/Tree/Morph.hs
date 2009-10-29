module Container.Tree.Morph where

import Generics.Morphism.Apo
import Generics.Morphism.Para
import qualified Container.Tree.Abstract as F

-- Insert is WRONG! see EQ case that throws existing k v. 

insert :: Ord k => k -> v -> CoEndoA (F.Tree k v)
insert k v s =
  case s of
    F.Branch m w l r ->
      case k `compare` m of
        LT -> F.Branch m w (Left l)               (Right (Left r))
        EQ -> F.Branch k v (Right (Left l))       (Left r)
        GT -> F.Branch m w (Right (Left l))       (Right (Left r))
    F.Leaf -> F.Branch k v (Right (Right F.Leaf)) (Right (Right F.Leaf))

fromList :: PhiA [(k, v)] (F.Tree k v)
fromList = Phi $ \f ->
  case f of
    []        -> F.Leaf
    (k, v):xs ->
      let l = take (length xs `div` 2) xs
          r = drop (length l) xs
      in F.Branch k v (Left l) (Left r)

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

