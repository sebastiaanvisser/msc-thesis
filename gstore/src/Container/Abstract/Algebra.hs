module Container.Abstract.Algebra where

import Generic.Representation
import qualified Container.Abstract.Tree as F

type Catamorphism a b c = (c, a -> b -> c -> c -> c)
type Paramorphism a b c = (Fix (F.Tree a b) -> Fix (F.Tree a b) -> c, a -> b -> c -> c -> Fix (F.Tree a b) -> Fix (F.Tree a b) -> c)

cata :: Catamorphism a b c -> Fix (F.Tree a b) -> c
cata c@(x, _) (In F.Leaf)             = x
cata c@(_, y) (In (F.Branch a b l r)) = y a b (cata c l) (cata c r)

para :: Paramorphism a b c -> Fix (F.Tree a b) -> c
para c@(x, _) (In F.Leaf)             = x (In F.Leaf) (In F.Leaf)
para c@(_, y) (In (F.Branch a b l r)) = y a b (para c l) (para c r) l r

algCount :: Catamorphism a b Integer
algCount = (0, (const . const) (\l r -> 1 + l + r))

algDepth :: Catamorphism a b Integer
algDepth = (0, (const . const) (\l r -> 1 + max l r))

algLookup :: (Monad m, Ord a) => a -> Catamorphism a b (m b)
algLookup a = (def, op)
  where
    def = fail "element not found"
    op c d l r =
      case a `compare` c of
        EQ -> return d
        LT -> r
        GT -> l

algInsert :: Ord a => a -> b -> Paramorphism a b (Fix (F.Tree a b))
algInsert a b = (def, op)
  where
    def n m = In $ F.Branch a b n m
    op c d l r x y =
      let (u, v) = if a > c then (l, y) else (x, r)
      in In (F.Branch c d u v)

