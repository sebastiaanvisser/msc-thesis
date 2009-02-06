module Tree where

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

fSelect :: (Ord a, Monad m) => (a -> f -> m b) -> a -> FTree a b f -> m b
fSelect f a Leaf = fail "element not found"
fSelect f a (Branch c d l r) =
  case a `compare` c of
    EQ -> return d
    LT -> f a l
    GT -> f a r

annSelect
  :: (Ord a, Monad m)
  => (m b -> c)    -- lifter for query result
  -> (a -> f -> c) -- recursive annotated selection
  -> a             -- key to search for
  -> FTree a b f   -- tree to search in
  -> c             -- lifted query result
annSelect p f a Leaf = p $ fail "element not found"
annSelect p f a (Branch c d l r) =
  case a `compare` c of
    EQ -> p (return d)
    LT -> f a l
    GT -> f a r

-- Real domain.

newtype Tree a b = Tree { ftree :: Fix (FTree a b) }

withFTree :: (Fix (FTree a b) -> Fix (FTree c d)) -> Tree a b -> Tree c d
withFTree f = Tree . f . ftree

empty :: Tree a b
empty = Tree (In fempty)

insert :: Ord a => a -> b -> Tree a b -> Tree a b
insert a b = withFTree $ fixM (\f -> finsert In (\a b -> f) a b)

insertWith :: Ord a => (b -> a) -> b -> Tree a b -> Tree a b
insertWith f a = insert (f a) a

select :: (Ord a, Monad m) => a -> Tree a b -> m b
select a = fixQ (\f -> fSelect (const f) a) . ftree

traceSelect :: (Ord a, Monad m) => a -> Tree a b -> (m b, [a])
traceSelect k = annQ lift post (\l q -> annSelect l (const q) k) . ftree
  where lift c a      = (a, [key (out c)])
        post c (a, b) = (a, key (out c) : b)

