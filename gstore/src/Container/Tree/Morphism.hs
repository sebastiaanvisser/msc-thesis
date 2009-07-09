module Container.Tree.Morphism where

import Prelude hiding (replicate, lookup)
import Generics.Representation
import qualified Container.Tree.Abstract as F

-- Tree morphism type signatures.

type Anamorphism      a b s   = s -> Maybe (a, b, s, s)
type Catamorphism     a b c   = (c, a -> b -> c -> c -> c)
type GParamorphism    a b c d = (Fix (F.Tree a b) -> Fix (F.Tree a b) -> c, a -> b -> d -> d -> Fix (F.Tree a b) -> Fix (F.Tree a b) -> c)
type Paramorphism     a b c   = GParamorphism a b c c
type Endoparamorphism a b     = GParamorphism a b (F.Tree a b (Fix (F.Tree a b))) (Fix (F.Tree a b))

-- Morphism implementations.

ana :: Anamorphism a b s -> s -> Fix (F.Tree a b)
ana f s =
  case f s of
    Nothing           -> In F.Leaf
    Just (a, b, u, v) -> In $ F.Branch a b (ana f u) (ana f v)

cata :: Catamorphism a b c -> Fix (F.Tree a b) -> c
cata c@(x, _) (In F.Leaf)             = x
cata c@(_, y) (In (F.Branch a b l r)) = y a b (cata c l) (cata c r)

para :: Paramorphism a b c -> Fix (F.Tree a b) -> c
para c@(x, _) (In F.Leaf)             = x (In F.Leaf) (In F.Leaf)
para c@(_, y) (In (F.Branch a b l r)) = y a b (para c l) (para c r) l r

endopara :: Endoparamorphism a b -> Fix (F.Tree a b) -> Fix (F.Tree a b)
endopara c@(x, _) (In F.Leaf)             = In $ x (In F.Leaf) (In F.Leaf)
endopara c@(_, y) (In (F.Branch a b l r)) = In $ y a b (endopara c l) (endopara c r) l r

-- Tree functions.

replicate :: Integral i => a -> b -> Anamorphism a b i
replicate _ _ 0 = Nothing
replicate a b i =
  let j = (i - 1) `div` 2
  in Just (a, b, j, i - 1 - j)

fromList :: Anamorphism a b [(a, b)]
fromList [] = Nothing
fromList ((a, b):ab) =
  let i = length ab `div` 2
  in Just (a, b, take i ab, drop i ab)

count :: Num i => Catamorphism a b i
count = (0, (const . const) (\l r -> 1 + l + r))

depth :: (Ord i, Num i) => Catamorphism a b i
depth = (0, (const . const) (\l r -> 1 + max l r))

lookup :: (Monad m, Ord a) => a -> Catamorphism a b (m b)
lookup a = (def, op)
  where
    def = fail "element not found"
    op c d l r =
      case a `compare` c of
        EQ -> return d
        LT -> r
        GT -> l

insert :: Ord a => a -> b -> Endoparamorphism a b
insert a b = (def, op)
  where
    def n m = F.Branch a b n m
    op c d l r x y =
      let (u, v) = if a > c then (l, y) else (x, r)
      in F.Branch c d u v




{-

balance t | depth Left  t > depth Right t + 1 = rotateRight
          | depth Right t > depth Left  t + 1 = rotateLeft
          | otherwise                         = id

rotateRight (Branch k0 v0 (Branch k1 v1 l r) rr) | depth Left 

-}








mytest =
    cata     count
  $ endopara (insert 'a' 'z')
  $ ana      (replicate '1' 'f')
    8

