module Tree where

import Prelude hiding (lookup)
import Storage

-- Fixpoint parametrized domain.

data FTree a b f =
    Leaf
  | Branch {key :: a, val :: b, left :: f, right :: f}
  deriving (Eq, Ord, Show, Read)

fempty :: FTree a b f
fempty = Leaf

finsert :: Ord a => (FTree a b f -> f) -> (a -> b -> f -> f) -> a -> b -> FTree a b f -> FTree a b f
finsert p f a b Leaf = Branch a b (p Leaf) (p Leaf)
finsert p f a b (Branch c d l r)
   | a > c     = Branch c d l (f a b r)
   | otherwise = Branch c d (f a b l) r

fromList :: Ord a => [(a, b)] -> Tree a b
fromList = foldr (uncurry insert) empty

flookup :: (Ord a, Monad m) => (a -> f -> m b) -> a -> FTree a b f -> m b
flookup f a Leaf = fail "element not found"
flookup f a (Branch c d l r) =
  case a `compare` c of
    EQ -> return d
    LT -> f a l
    GT -> f a r

alookup
  :: (Ord a, Monad m)
  => (m b -> c)    -- lifter for query result
  -> (a -> f -> c) -- recursive alookup
  -> a             -- key to search for
  -> FTree a b f   -- tree to search in
  -> c             -- lifted query result
alookup p f a Leaf = p $ fail "element not found"
alookup p f a (Branch c d l r) =
  case a `compare` c of
    EQ -> p (return d)
    LT -> f a l
    GT -> f a r

-- Recursive domain.

type Tree a b = Fix (FTree a b)

empty :: Tree a b
empty = In fempty

insert :: Ord a => a -> b -> Tree a b -> Tree a b
insert a b = fixM (\f -> finsert In (\a b -> f) a b)

insertWith :: Ord a => (b -> a) -> b -> Tree a b -> Tree a b
insertWith f a = insert (f a) a

lookup :: (Ord a, Monad m) => a -> Tree a b -> m b
lookup a = fixQ (\f -> flookup (const f) a)

traceLookup :: Ord a => a -> Tree a b -> (Maybe b, [a])
traceLookup k = post . annQ
  (\c a      -> (a, [c]))
  (\c (a, b) -> (a, c:b))
  (\lift rec -> alookup lift (\a -> rec) k)
    where post (a, b) = (a, map (key . out) b)


