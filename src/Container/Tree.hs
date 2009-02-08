module Container.Tree where

import Prelude hiding (lookup)
import Generic.Annotate

-- Fixpoint parametrized domain.

data FTree a b f =
    Leaf
  | Branch {key :: a, val :: b, left :: f, right :: f}
  deriving (Eq, Ord, Show, Read)

fempty :: FTree a b f
fempty = Leaf

finsert :: Ord a => (FTree a b f -> f) -> (a -> b -> f -> f) -> a -> b -> FTree a b f -> FTree a b f
finsert p _ a b Leaf = Branch a b (p Leaf) (p Leaf)
finsert _ f a b (Branch c d l r)
   | a > c     = Branch c d l (f a b r)
   | otherwise = Branch c d (f a b l) r

fromList :: Ord a => [(a, b)] -> Tree a b
fromList = foldr (uncurry insert) empty

fLookup :: (Ord a, Monad m) => (a -> f -> m b) -> a -> FTree a b f -> m b
fLookup _ _ Leaf = fail "element not found"
fLookup f a (Branch c d l r) =
  case a `compare` c of
    EQ -> return d
    LT -> f a l
    GT -> f a r

annLookup
  :: (Ord a, Monad m)
  => (m b -> c)    -- lifter for query result
  -> (a -> f -> c) -- recursive annotated lookup
  -> a             -- key to search for
  -> FTree a b f   -- tree to search in
  -> c             -- lifted query result
annLookup p _ _ Leaf = p $ fail "element not found"
annLookup p f a (Branch c d l r) =
  case a `compare` c of
    EQ -> p (return d)
    LT -> f a l
    GT -> f a r

-- Real domain.

newtype Tree a b = Tree { ftree :: Fix (FTree a b) }
  deriving Show

withFTree :: (Fix (FTree a b) -> Fix (FTree c d)) -> Tree a b -> Tree c d
withFTree f = Tree . f . ftree

empty :: Tree a b
empty = Tree (In fempty)

insert :: Ord a => a -> b -> Tree a b -> Tree a b
insert a b = withFTree $ fixM (\f -> finsert In (\_ _ -> f) a b)

insertWith :: Ord a => (b -> a) -> b -> Tree a b -> Tree a b
insertWith f a = insert (f a) a

--

lookup :: (Ord a, Monad m) => a -> Tree a b -> m b
lookup a = fixQ (\f -> fLookup (const f) a) . ftree

traceLookup :: (Ord a, Monad m) => a -> Tree a b -> (m b, [a])
traceLookup a = traceQ (\l q -> annLookup l (const q) a) key . ftree

ioLookup :: (Ord a, Monad m) => a -> Tree a b -> IO (m b)
ioLookup a = ioQ (\l q -> annLookup l (const q) a) . ftree

